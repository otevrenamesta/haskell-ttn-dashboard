let
  proj = import ./app.nix {};
in
  proj.ttn-client
