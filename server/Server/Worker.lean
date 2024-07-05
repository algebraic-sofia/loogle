import Lean.Data.Json
import Server.Loogle
import Server.Data

open Lean Parser
open Server.Loogle

/-- The Chan type is used to transport requests back and forth -/
def Chan := (IO.Channel String × IO.Channel (Except String Data))

def Chan.new : IO Chan := Prod.mk <$> IO.Channel.new <*> IO.Channel.new

def Chan.request (channel: Chan) (query: String) : IO (Except String Data) := do
  channel.1.send query
  match ← channel.2.sync.recv? with
  | .none     => .error "Child not responding"
  | .some res => pure res

def childConfig : IO.Process.SpawnArgs := {
      cmd := "./.lake/build/bin/loogle",
      args := #["--json", "--interactive"],
      stdin := .piped,
      stdout := .piped,
      stderr := .piped
    }

def runQuery (child : IO.Process.Child (IO.Process.SpawnArgs.toStdioConfig childConfig)) (query : String) : IO String := do
  -- Write the query to the process's stdin
  child.stdin.write ((query ++ "\n").toUTF8)
  -- Read the response from the process's stdout
  let response ← child.stdout.readToEnd
  pure response

/-- Runs a Task that will do queries and send to another channel. Its used because queries
only perform well inside the `work` function. -/
def startWorker : IO Chan := do
  let chan ← Chan.new
  let ready ← IO.Promise.new

  let _ ← IO.asTask $ do
    let loogle ← startLoogle
    let loogleRef ← IO.mkRef loogle
    let result ← waitStart loogleRef

    ready.resolve result

    for queryStr in chan.1.sync do
      let res ← query loogleRef queryStr.trim
      match res with
      | .ok res => chan.2.send (FromJson.fromJson? res)
      | .killed res => chan.2.send (.error $ toString res)

  let answer ← IO.wait ready.result

  unless answer do
    throw $ IO.userError "Cannot start worker process for loogle."

  return chan
