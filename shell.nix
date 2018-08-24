let
  proj = import ./app.nix {};
in
  proj.server.env
