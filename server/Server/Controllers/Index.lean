import Http
import Server
import Server.Views.Index
import Server.Query
import Server.Handlers.Metrics

namespace Server.Controllers

open Lean
open Http Http.Data Http.Data.Headers

def sanitizeQuery (s: String) : String :=
  if s.startsWith "#find "
    then s.drop 6
    else s

def indexPage (conn: Connection α) : Handler Unit := do
  let data := Uri.Query.fromString =<< conn.request.uri.query
  let query := (Uri.Query.find? · "q") =<< data
  let lucky := (Uri.Query.find? · "lucky") =<< data

  let query := URI.componentDecode =<< query

  if query.isSome then
    Handler.metricsAddAgent false conn

  let result ← queryData query

  if lucky.isSome then
    if let some hit := result.bind (·.hits |>.get? 0) then
      redirect conn (Components.docLink hit)

  let data := Server.Views.index
    query
    result
    (← Handler.get (·.loogleRev))
    (← Handler.get (·.mathlibRev))

  html conn (String.toUTF8 (toString data))
