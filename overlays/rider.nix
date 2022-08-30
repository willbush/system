self: super:
{
  jetbrains = super.jetbrains // {
    rider = super.jetbrains.rider.overrideAttrs (_: rec {
      # versions >= 2022.2 crash for me on startup. I haven't had time to look into why yet.
      version = "2022.1.2";
      name = "rider-${version}";

      src = super.fetchurl {
        url = "https://download.jetbrains.com/rider/JetBrains.Rider-${version}.tar.gz";
        sha256 = "4513e55d3db013986bdfda52854f89fb127a153f2588ae456d6e7a182987b741";
      };
    });
  };
}
