import Http
import Server
import Server.Views.Index
import Server.Query

namespace Server.Controllers

open Lean
open Http Http.Data Http.Data.Headers

def metricsPage (conn: Connection α) : Handler Unit := do
  let metrics ← Handler.metrics
  plain conn (toString (← metrics.get))
  return
