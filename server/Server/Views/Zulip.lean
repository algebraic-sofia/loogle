import Server.Context
import Http.Data.URI

namespace Server.Views
open Lean Http.Data

def docLink (hit : Hit) : String :=
  let name := hit.name.split (· == '.')
              |>.map URI.encode
              |> String.intercalate "/"
  let modpath := hit.module.split (· == '.')
              |>.map URI.encode
              |> String.intercalate "/"
  let baseUrl := "https://leanprover-community.github.io/mathlib4_docs/"
  s!"{baseUrl}{modpath}.html#{name}"

def queryLink (query : String) : String :=
  let baseUrl := "https://loogle.lean-lang.org/?q="
  s!"{baseUrl}{URI.encode query}"

def zulHit (hit : Hit) : String :=
  let name := hit.name
  let link := docLink hit
  s!"[{name}]({link})"

def zulQuery (sugg : String) : String :=
  let queryLinkStr := queryLink sugg
  s!"[`{sugg}`]({queryLinkStr})"


def formatError (err: String) : String :=
  if err.contains '\n' then
    s!"❗\n```\n{err}\n```"
  else
    s!"❗ {err}"

def formatSuggestions (suggs: Array String) : String :=
  match suggs.size with
  | 1 => s!" Did you mean {zulQuery $ suggs.getD 0 ""}?"
  | 2 => s!" Did you mean {zulQuery $ suggs.getD 0 ""} or {zulQuery $ suggs.getD 1 ""}?"
  | _ => s!" Did you mean {zulQuery $ suggs.getD 0 ""}, {zulQuery $ suggs.getD 1 ""}, or something else?"

def formatHits (query: String) (size: Nat) (hits: Array Hit) : String :=
  match hits.size with
  | 0 => "🤷 nothing found"
  | 1 => s!"🔍 {zulHit $ hits.get! 0}"
  | 2 => s!"🔍 {zulHit $ hits.get! 0}, {zulHit $ hits.get! 1}"
  | _ => s!"🔍 {zulHit $ hits.get! 0}, {zulHit $ hits.get! 1}, and [{size - 2} more]({queryLink query})"

def zulipView (query: String) (result: Data) : String := Id.run do
  match result.error with
  | some err =>
    let reply := formatError err
    match result.suggestions with
    | some suggs => reply ++ formatSuggestions suggs
    | none => reply
  | none => formatHits query result.count result.hits
