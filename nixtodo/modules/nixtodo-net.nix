{
  network.description = "nixtodo network";

  defaults = <nixtodo/modules/base.nix>;

  backend = {
    services.nixtodo.enable = true;
  };
}
