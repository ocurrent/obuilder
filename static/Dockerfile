# escape=`
FROM mcr.microsoft.com/windows/servercore:20H2
ARG CYGWIN_ROOT
USER ContainerAdministrator
ENV CYGWIN="winsymlinks:native"
ADD [ "https://www.cygwin.com/setup-x86_64.exe", "C:/cygwin-setup-x86_64.exe" ]
RUN mkdir %CYGWIN_ROOT%\lib\cygsympathy && mkdir %CYGWIN_ROOT%\etc\postinstall
ADD [ "https://raw.githubusercontent.com/metastack/cygsympathy/master/cygsympathy.cmd", "$CYGWIN_ROOT/lib/cygsympathy/" ]
ADD [ "https://raw.githubusercontent.com/metastack/cygsympathy/master/cygsympathy.sh", "$CYGWIN_ROOT/lib/cygsympathy/cygsympathy" ]
RUN mklink %CYGWIN_ROOT%\etc\postinstall\zp_zcygsympathy.sh %CYGWIN_ROOT%\lib\cygsympathy\cygsympathy
RUN C:\cygwin-setup-x86_64.exe --quiet-mode --no-shortcuts --no-startmenu `
    --no-desktop --only-site --local-package-dir %TEMP% --root %CYGWIN_ROOT% `
    --site http://mirrors.kernel.org/sourceware/cygwin/ `
    --packages tar
COPY [ "extract.cmd", "C:/extract.cmd" ]
