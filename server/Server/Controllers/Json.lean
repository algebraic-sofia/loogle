import Http
import Server
import Server.Views.Index
import Server.Query
import Server.Handlers.Metrics

namespace Server.Controllers

open Lean
open Http Http.Data Http.Data.Headers

def jsonPage (conn: Connection α) : Handler Unit := do
  Handler.metricsAddAgent true conn

  let query := queryParam conn

  if query.isNone then
    json conn (Json.obj RBNode.leaf)
    return

  let result ← queryData query

  match result with
  | some res => json conn (toJson res)
  | none => internalError conn
