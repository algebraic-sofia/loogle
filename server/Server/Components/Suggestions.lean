import Server.Jsx
import Server.Context
import Http.Data.URI

namespace Server.Components
open Server.Jsx
open Http.Data

def localLink (query: String) : String :=
  s!"/?q={URI.encode query}"

def suggestionComponent (sugg: String) : Content :=
  <li>
    "🔍"
    <a href={localLink sugg}>
      <code> {sugg} </code>
    </a>
  </li>

def suggestionsComponent (suggestions: Array String) : Element :=
  let suggs := suggestions.map suggestionComponent
  <>
    <h2>{"Did you maybe mean"}</h2>
    <ul> [suggs] </ul>
  </>
