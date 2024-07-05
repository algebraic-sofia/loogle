import Http
import Server
import Server.Views.Zulip
import Server.Query

namespace Server.Controllers

open Lean
open Http Http.Data Http.Data.Headers
open Server.Views

structure ZulipMessage where
  data: String
  deriving ToJson, FromJson

def findSubstring? (s : String) (substr : String) : Option Nat := do
  let len := substr.length

  for i in [0:(s.length-len) + 1] do
    if i + len ≤ s.length ∧ s.extract (String.Pos.mk i) (String.Pos.mk $ i + len) == substr then
      return i

  none

def extractLoogleCommand (message : String) : Option String := do
  let prefix_ := "**loogle**"
  let data := message
  let startIndex := findSubstring? data prefix_
  match startIndex with
  | none => none
  | some i => do
    let startIdx := i + prefix_.length
    let mut commandStart := startIdx

    for idx in [startIdx:data.length] do
      match data.get? $ String.Pos.mk idx with
      | some ' ' | some ':' | some '?' | some ',' => continue
      | _ =>
        commandStart := idx
        break

    let command := data.drop commandStart
    if command.isEmpty then none else some command.trim

def zulipBotPage (conn: Connection (Array ByteArray)) : Handler Unit := do
  if conn.request.headers.findRaw? "content-type" ≠ some "application/json" then
    badRequest conn
    return

  let .ok message ← (FromJson.fromJson? =<< ·) <$> (getJson conn)
    | badRequest conn
      return

  let metrics ← Handler.metrics
  Metrics.updateClients metrics "zulip"

  let data := ZulipMessage.data message
  let data := extractLoogleCommand data |>.getD (data.split ('\n' == ·) |>.headD "")

  let .ok result ← Handler.request data
    | internalError conn
      return

  json conn (json% { "content": $(zulipView data result) })
