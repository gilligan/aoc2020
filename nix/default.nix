let
  sources = import ./sources.nix;

  overlays = [
    (pkgs: _: rec {
      nLib = rec {
        collectTargets = root:
          let
            hasOutPath = x: x ? outPath;
          in
          (if hasOutPath root then [ root ] else
          (if builtins.typeOf root == "set" then pkgs.lib.concatMap collectTargets (pkgs.lib.attrValues root) else [ ]));

        mkGcRoot = root:
          let
            paths = pkgs.lib.unique (collectTargets root);
            drvPaths = map (p: p.drvPath) (builtins.filter (p: p ? drvPath) paths);
          in
          pkgs.runCommand "gcroot" { } (''
            mkdir $out
          '' + (pkgs.lib.concatMapStrings (p: "ln -s ${p} $out\n") (paths ++ drvPaths)));
      };
    })
  ];

in
import sources.nixpkgs { inherit overlays; }
