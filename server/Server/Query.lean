import Http
import Server
import Server.Views.Index

open Lean
open Http Http.Data Http.Data.Headers

def queryParam (conn: Connection α) : Option String := do
  (Uri.Query.find? · "q")
    =<< Uri.Query.fromString
    =<< conn.request.uri.query

def queryData (query: Option String) : Handler (Option Data) := do
  if let some query := query
    then do
      let metrics ← Handler.metrics
      match ← Handler.request query with
      | .ok res => do
        Metrics.updateHeartbeats metrics res.heartbeats
        Metrics.updateResults metrics res.count
        pure res
      | .error _ => do
        Metrics.updateErrors metrics
        pure none
    else pure none
