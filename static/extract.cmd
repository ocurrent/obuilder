@echo off

echo Copying to "%DESTINATION%"
copy /v /b C:\manifest.bash "%DESTINATION%"
copy /v /b C:\cygwin64\bin\basename.exe "%DESTINATION%"
copy /v /b C:\cygwin64\bin\bash.exe "%DESTINATION%"
copy /v /b C:\cygwin64\bin\cygpath.exe "%DESTINATION%"
copy /v /b C:\cygwin64\bin\readlink.exe "%DESTINATION%"
copy /v /b C:\cygwin64\bin\tar.exe "%DESTINATION%"
copy /v /b C:\cygwin64\bin\sha256sum.exe "%DESTINATION%"

for /f "usebackq delims=" %%f in (`C:\cygwin64\bin\bash.exe -lc "ldd -- /bin/basename.exe /bin/bash.exe /bin/cygpath.exe /bin/readlink.exe /bin/tar.exe /bin/sha256sum.exe | sed -ne 's|.* => \(/usr/bin/.*\) ([^)]*)$|\1|p' | sort -u | xargs cygpath -w"`) do (
  copy /v /b "%%f" "%DESTINATION%"
)
