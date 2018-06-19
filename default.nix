with import <nixpkgs> {};

let
  erlang = erlangR20;
in

stdenvNoCC.mkDerivation {
  name = "derivery";
  buildInputs = [ erlang (rebar3.override { inherit erlang; }) ];
}
