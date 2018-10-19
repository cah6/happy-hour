{
  network.description = "Happy hour servers";

  backend1 =
    { config, pkgs, ... }: 
    let
      backend-servant = (import ./release.nix {}).backend-servant;
    in
    { 
      networking.hostName = "backend1";

      networking.firewall.allowedTCPPorts = [ 22 80 3000 ];
      environment.systemPackages = [ backend-servant ];

      systemd.services.backend-servant =
        { description = "Happy Hour Webserver";
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" ];
          serviceConfig =
            { ExecStart = "${backend-servant}/bin/backend-servant";
            };
        };
    };
}