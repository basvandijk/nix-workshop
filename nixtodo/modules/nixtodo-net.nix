let
  region      = "eu-west-1";
  accessKeyId = "todo-list-app";
in rec {
  network.description = "nixtodo network";

  defaults = <nixtodo/modules/base.nix>;

  backend = {resources, ... }: {
    deployment = {
      targetEnv = "ec2";
      ec2 = {
        inherit region accessKeyId;
        instanceType = "t2.micro";
        ebsInitialRootDiskSize = 10; # GB
        keyPair     = resources.ec2KeyPairs.todo-list-app-key-pair;
        elasticIPv4 = resources.elasticIPs.nixtodo-elastic-ip;
      };
    };
    services.nixtodo.enable = true;
  };

  resources = {
    ec2KeyPairs.todo-list-app-key-pair =
      { inherit region accessKeyId; };

    elasticIPs.nixtodo-elastic-ip =
      { inherit region accessKeyId; };
  };
}
