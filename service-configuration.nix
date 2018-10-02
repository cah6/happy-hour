{
  network.description = "Happy hour servers";

  happy-hour =
    { config, pkgs, ... }: 
    let
      happy-hour = (import ./release.nix {}).project1;
    in
    { 
      networking.hostName = "happy-hour";

      networking.firewall.allowedTCPPorts = [ 22 80 3000 ];
      environment.systemPackages = [ happy-hour ];

      systemd.services.happy-hour =
        { description = "Happy Hour Webserver";
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" ];
          serviceConfig =
            { ExecStart = "${happy-hour}/bin/happy-hour-backend";
            };
        };
    };
}