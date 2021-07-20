@echo off

copy %CYGWIN_ROOT%\bin\basename.exe %DESTINATION%
copy %CYGWIN_ROOT%\bin\bash.exe %DESTINATION%
copy %CYGWIN_ROOT%\bin\head.exe %DESTINATION%
copy %CYGWIN_ROOT%\bin\readlink.exe %DESTINATION%
copy %CYGWIN_ROOT%\bin\tar.exe %DESTINATION%
copy %CYGWIN_ROOT%\bin\sha256sum.exe %DESTINATION%

for /f "usebackq delims=" %%f in (`%CYGWIN_ROOT%\bin\bash -lc "ldd -- /bin/basename.exe /bin/bash.exe /bin/head.exe /bin/readlink.exe /bin/tar.exe /bin/sha256sum.exe | sed -ne 's|.* => \(/usr/bin/.*\) ([^)]*)$|\1|p' | sort -u | xargs cygpath -w"`) do (
  echo Copying %%f
  copy %%f %DESTINATION%
)
