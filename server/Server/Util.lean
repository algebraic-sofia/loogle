import Http
import Server.Context
import Lean.Data.Json

open Lean
open Http Http.Data Http.Data.Headers

def internalError (conn: Connection α) : Handler Unit := do
  conn.setStatus .internalServerError
  conn.write "Internal Error."
  conn.end

def redirect (conn: Connection α) (url: String) : Handler Unit := do
  conn.setStatus .found
  conn.withHeader "Location" url
  conn.end

def json (conn: Connection α) (json: Json) : Handler Unit := do
  conn.withHeader "Content-Type" "application/json"
  conn.write (toString json)
  conn.end

def plain (conn: Connection α) (json: String) : Handler Unit := do
  conn.withHeader "Content-Type" "text/plain"
  conn.write json
  conn.end

def png (conn: Connection α) (data: ByteArray) : Handler Unit := do
  conn.setStatus .ok
  conn.withHeader "Content-Type" "image/png"
  conn.writeByteArray data
  conn.end

def html (conn: Connection α) (data: ByteArray) : Handler Unit := do
  conn.setStatus .ok
  conn.withHeader "Content-Type" "text/html"
  conn.writeByteArray data
  conn.end

def notFound (conn: Connection α) : Handler Unit := do
  conn.setStatus .notFound
  conn.withHeader "Content-Type" "text/plain"
  conn.write "Not found."
  conn.end

def badRequest (conn: Connection α) : Handler Unit := do
  conn.setStatus .badRequest
  conn.withHeader "Content-Type" "text/plain"
  conn.write "Bad Request."
  conn.end

def getJson (conn: Connection (Array (ByteArray))) : IO (Except String Json) := do
  let data ← conn.data.swap #[]
  let data := data.foldl ByteArray.append ByteArray.empty
  if let some data := String.fromUTF8? data
    then return Json.parse data
    else return Except.error "cannot read data"
