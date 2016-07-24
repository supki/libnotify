{ mkDerivation, base, bytestring, glib, gtk, libnotify, stdenv }:
mkDerivation {
  pname = "libnotify";
  version = "0.1.1.0";
  src = ./.;
  libraryHaskellDepends = [ base bytestring glib gtk ];
  librarySystemDepends = [ libnotify ];
  description = "Bindings to libnotify library";
  license = stdenv.lib.licenses.mit;
}
