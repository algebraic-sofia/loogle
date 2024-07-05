import Server.Context
import Http

namespace Server.Handler
open Http
open Lean

def metricsAddAgent (isJson: Bool) (conn: Connection α) : Handler Unit := do
  let metrics ← Handler.metrics
  let loogleClient := conn.request.headers.findD "x-loogle-client" ""
  let userAgent := conn.request.headers.findD "user-agent" ""

  if isJson then
    if loogleClient.startsWith "lean4/" then
      Metrics.updateClients metrics "vscode-lean4"
    else if userAgent = "vscode" then
      Metrics.updateClients metrics "vscode-loogle"
    else if userAgent = "lean.nvim" then
      Metrics.updateClients metrics "nvim"
    else if userAgent = "lean+nvim" then
      Metrics.updateClients metrics "nvim"
    else
      Metrics.updateClients metrics "json"
  else
    Metrics.updateClients metrics "web"
