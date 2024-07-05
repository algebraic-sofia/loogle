import Http
import Server
import Server.Metrics
import Lean.Data.Json

import Server.Controllers.Index
import Server.Controllers.Zulip
import Server.Controllers.Metrics
import Server.Controllers.Json

open Lean
open Server.Controllers
open Http Http.Data Http.Data.Headers

def readError (conn: Connection α) (data: Handler Unit) : Handler Unit :=
  try data
  catch err => do
    IO.println s!"internal error: {err}"
    internalError conn

def router (conn: Connection (Array ByteArray)) : Handler Unit := do
  match (conn.request.method, conn.request.uri.path) with
  | (.get, "/")           => indexPage conn
  | (.get, "/json")       => jsonPage conn
  | (.get, "/metrics")    => metricsPage conn
  | (.post, "/zulipbot")  => zulipBotPage conn
  | (.get, "/loogle.png") => png conn (← Handler.static! "icon")
  | _ => notFound conn

def accumulateData (conn: Connection (Array ByteArray)) (data: Chunk) : IO Unit := do
  conn.data.modify (λx => x.push data.data)

def main : IO Unit := do
  IO.println "Starting Worker"
  let chan ← startWorker
  IO.println "Starting Server"

  let staticData ← Static.ofFiles #[("icon", "./assets/loogle.png")]

  let loogleRev ← getGitRevision
  let mathlibRev ← getMathlibRevision
  let metrics ← initializeMetrics loogleRev mathlibRev
  let ctx := Context.mk metrics staticData chan loogleRev mathlibRev

  IO.println s!"Starting server {loogleRev} {mathlibRev}"

  Http.IO.Server.server
    (data := (#[] : Array ByteArray))
    (host := "127.0.0.1")
    (port := 8081)
    (onEnd := λconn _ => (ReaderT.run (readError conn $ router conn) ctx))
    (onData := accumulateData)
