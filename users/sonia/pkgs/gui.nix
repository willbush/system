{ pkgs, ... }:
{
  home.packages = with pkgs; [
    # java
    jdk8
    jetbrains.idea-community
  ];
}
