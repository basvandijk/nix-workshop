pkgs : drv : with pkgs.haskell.lib;
overrideCabal drv (drv : {
    postInstall = ''
      mkdir -p $out/static
      ln -s ${./static/style.css}    $out/static/style.css
      ln -s ${./static/favicon.ico}  $out/static/favicon.ico
      ln -s ${./index.html.mustache} $out/index.html.mustache
    '';
  })
