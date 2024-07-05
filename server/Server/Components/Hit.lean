import Server.Jsx
import Server.Context
import Http.Data.URI

namespace Server.Components
open Server.Jsx
open Http.Data

def docLink (hit : Hit) : String :=
  let name := hit.name.split (· == '.')
              |>.map URI.encode
              |> String.intercalate "/"
  let modpath := hit.module.split (· == '.')
              |>.map URI.encode
              |> String.intercalate "/"
  let baseUrl := "https://leanprover-community.github.io/mathlib4_docs/"
  s!"{baseUrl}{modpath}.html#{name}"

def hitComponent (hit: Hit) : Element :=
  <li>
    <a href={docLink hit}>
      {hit.name}
    </a>
    {" "}
    <small>
      {hit.module}
    </small>
    <br />
    <tt>
      {hit.type}
    </tt>
  </li>
