{ pkgs, ... }: {
  home = with pkgs; {
    packages = [
      dotnet-sdk
    ];
    sessionVariables = {
      DOTNET_CLI_TELEMETRY_OPTOUT = 1;
      # I thought this would help Rider found dotnet, but it doesn't seem to work.
      DOTNET_ROOT = dotnet-sdk;
    };
  };
}
