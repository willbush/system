{
  xdg = {
    enable = true;
    mime.enable = true;
    mimeApps = {
      enable = true;
      # query mime type from a file like this:
      # xdg-mime query filetype your-file.extension
      # also check out:
      # https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types/Common_types
      defaultApplications = {
        "application/msword" = "writer.desktop";
        "application/pdf" = "org.pwmt.zathura.desktop";
        "application/vnd.oasis.opendocument.text" = "writer.desktop";
        "application/vnd.openxmlformats-officedocument.wordprocessingml.document" = "writer.desktop";

        "application/vnd.ms-excel" = "calc.desktop";
        "application/vnd.oasis.opendocument.spreadsheet" = "calc.desktop";
        "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" = "calc.desktop";

        "audio/aac" = "mpv.desktop";
        "audio/mpeg" = "mpv.desktop";
        "audio/ogg" = "mpv.desktop";
        "audio/wav" = "mpv.desktop";
        "audio/webm" = "mpv.desktop";
        "audio/x-midi" = "mpv.desktop";

        "image/gif" = "sxiv.desktop";
        "image/jpeg" = "sxiv.desktop";
        "image/jpg" = "sxiv.desktop";
        "image/png" = "sxiv.desktop";
        "image/svg+xml" = "firefox.desktop";
        "image/tiff" = "sxiv.desktop";
        "image/vnd.microsoft.icon" = "sxiv.desktop";
        "image/webp" = "sxiv.desktop";

        # Open directory in Emacs dired.
        "inode/directory" = "emacsclient.desktop";

        "text/html" = "firefox.desktop";
        "text/plain" = "emacsclient.desktop";

        "video/mp4" = "vlc.desktop";
        "video/mpeg" = "vlc.desktop";
        "video/ogg" = "vlc.desktop";
        "video/webm" = "vlc.desktop";
        "video/x-matroska" = "vlc.desktop";
        "video/x-msvideo" = "vlc.desktop";

        "x-scheme-handler/about" = "firefox.desktop";
        "x-scheme-handler/http" = "firefox.desktop";
        "x-scheme-handler/https" = "firefox.desktop";
        "x-scheme-handler/unknown" = "firefox.desktop";
      };

      associations.added = {
        # These apps overwrite mimeapps.list on startup unless this is explicitly added
        "x-scheme-handler/magnet" = "transmission-gtk.desktop";
        "x-scheme-handler/tg" = "telegramdesktop.desktop";
      };
    };

    userDirs = {
      enable = true;
      desktop = "$HOME/desktop";
      download = "$HOME/downloads";
      videos = "$HOME/videos";
    };
  };
}
