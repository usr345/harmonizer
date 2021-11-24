{ pkgs ? import <nixpkgs> {} }:

let 
  genutils = builtins.fetchTarball {
    name = "swipl-pack-genutils";
    url = "https://raw.githubusercontent.com/samer--/prolog/master/genutils/release/genutils-0.3.8.tgz";
    sha256 = "0ychldkbapgsh82nn4yp7ddm760kcgw5j60wh4pd4wisca0wqwiq";
  };
  dcgutils = builtins.fetchTarball {
    name = "swipl-pack-dcgutils";
    url = "https://raw.githubusercontent.com/samer--/prolog/master/dcgutils/release/dcgutils-1.1.3.tgz";
    sha256 = "073xiqdzp53wisxv6s0xkc3njj2l79yfpmwv9ddk4wasz3zkcx97";
  };
  musicxml = builtins.fetchTarball {
    name = "swipl-pack-musicxml";
    url = "https://raw.githubusercontent.com/samer--/prolog/master/musicxml/release/musicxml-0.0.2.tgz";
    sha256 = "1absxm7ly9gyq7lil7ffby13x95mrwqa5fd2z3bv82pp90s73bi5";
  };
  extraPacks = map (dep-path: "'file://${dep-path}'") 
    [ genutils dcgutils musicxml ]; 
  prolog = pkgs.swiProlog.override { inherit extraPacks; };
in  
  
pkgs.mkShell {
  nativeBuildInputs = [ prolog ];
}
