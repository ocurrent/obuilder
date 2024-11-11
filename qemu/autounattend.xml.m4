<?xml version="1.0" encoding="utf-8"?>
<unattend xmlns="urn:schemas-microsoft-com:unattend">
    <settings pass="windowsPE">
        <component language="neutral" name="Microsoft-Windows-International-Core-WinPE" processorArchitecture="amd64" publicKeyToken="31bf3856ad364e35" versionScope="nonSxS" xmlns:wcm="http://schemas.microsoft.com/WMIConfig/2002/State" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
            <SetupUILanguage>
                <UILanguage>en-US</UILanguage>
            </SetupUILanguage>
            <InputLocale>en-US</InputLocale>
            <SystemLocale>en-US</SystemLocale>
            <UILanguage>en-US</UILanguage>
            <UILanguageFallback>en-US</UILanguageFallback>
            <UserLocale>en-US</UserLocale>
        </component>
        <component language="neutral" name="Microsoft-Windows-PnpCustomizationsWinPE" processorArchitecture="amd64" publicKeyToken="31bf3856ad364e35" versionScope="nonSxS" xmlns:wcm="http://schemas.microsoft.com/WMIConfig/2002/State" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
            <DriverPaths>
                <PathAndCredentials wcm:action="add" wcm:keyValue="1">
                    <Path>f:\NetKVM\w11\amd64\</Path>
                </PathAndCredentials>
                <PathAndCredentials wcm:action="add" wcm:keyValue="2">
                    <Path>f:\viostor\w11\amd64\</Path>
                </PathAndCredentials>
            </DriverPaths>
        </component>
        <component language="neutral" name="Microsoft-Windows-Setup" processorArchitecture="amd64" publicKeyToken="31bf3856ad364e35" versionScope="nonSxS" xmlns:wcm="http://schemas.microsoft.com/WMIConfig/2002/State" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
            <RunSynchronous>
                <RunSynchronousCommand wcm:action="add">
                    <Order>1</Order>
                    <Description>BypassTPMCheck</Description>
                    <Path>cmd /c reg add "HKLM\SYSTEM\Setup\LabConfig" /v "BypassTPMCheck" /t REG_DWORD /d 1</Path>
                </RunSynchronousCommand>
                <RunSynchronousCommand wcm:action="add">
                    <Order>2</Order>
                    <Description>BypassSecureBootCheck</Description>
                    <Path>cmd /c reg add "HKLM\SYSTEM\Setup\LabConfig" /v "BypassSecureBootCheck" /t REG_DWORD /d 1</Path>
                </RunSynchronousCommand>
                <RunSynchronousCommand wcm:action="add">
                    <Order>3</Order>
                    <Description>BypassRAMCheck</Description>
                    <Path>cmd /c reg add "HKLM\SYSTEM\Setup\LabConfig" /v "BypassRAMCheck" /t REG_DWORD /d 1</Path>
                </RunSynchronousCommand>
            </RunSynchronous>
            <DiskConfiguration>
                <Disk wcm:action="add">
                    <CreatePartitions>
                        <CreatePartition wcm:action="add">
                            <Type>Primary</Type>
                            <Order>1</Order>
                            <Size>350</Size>
                        </CreatePartition>
                        <CreatePartition wcm:action="add">
                            <Order>2</Order>
                            <Type>Primary</Type>
                            <Extend>true</Extend>
                        </CreatePartition>
                    </CreatePartitions>
                    <ModifyPartitions>
                        <ModifyPartition wcm:action="add">
                            <Active>true</Active>
                            <Format>NTFS</Format>
                            <Order>1</Order>
                            <PartitionID>1</PartitionID>
                        </ModifyPartition>
                        <ModifyPartition wcm:action="add">
                            <Format>NTFS</Format>
                            <Letter>C</Letter>
                            <Order>2</Order>
                            <PartitionID>2</PartitionID>
                        </ModifyPartition>
                    </ModifyPartitions>
                    <DiskID>0</DiskID>
                    <WillWipeDisk>true</WillWipeDisk>
                </Disk>
                <Disk wcm:action="add">
                    <CreatePartitions>
                        <CreatePartition wcm:action="add">
                            <Order>1</Order>
                            <Type>Primary</Type>
                            <Extend>true</Extend>
                        </CreatePartition>
                    </CreatePartitions>
                    <ModifyPartitions>
                        <ModifyPartition wcm:action="add">
                            <Format>NTFS</Format>
                            <Letter>Z</Letter>
                            <Order>1</Order>
                            <PartitionID>1</PartitionID>
                        </ModifyPartition>
                    </ModifyPartitions>
                    <DiskID>1</DiskID>
                    <WillWipeDisk>true</WillWipeDisk>
                </Disk>
            </DiskConfiguration>
            <ImageInstall>
                <OSImage>
                    <InstallFrom>
                        <MetaData wcm:action="add">
                            <Key>/IMAGE/NAME</Key>
                            <Value>Windows Server 2022 SERVERSTANDARDCORE</Value>
                        </MetaData>
                    </InstallFrom>
                    <InstallTo>
                        <DiskID>0</DiskID>
                        <PartitionID>2</PartitionID>
                    </InstallTo>
                    <WillShowUI>OnError</WillShowUI>
                </OSImage>
            </ImageInstall>
            <UserData>
                <ProductKey>
                    <WillShowUI>OnError</WillShowUI>
                </ProductKey>
                <AcceptEula>true</AcceptEula>
            </UserData>
        </component>
    </settings>
    <settings pass="oobeSystem">
        <component name="Microsoft-Windows-Shell-Setup" processorArchitecture="amd64" publicKeyToken="31bf3856ad364e35" language="neutral" versionScope="nonSxS" xmlns:wcm="http://schemas.microsoft.com/WMIConfig/2002/State" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
            <UserAccounts>
                <AdministratorPassword>
                    <Value>bwBwAGEAbQBBAGQAbQBpAG4AaQBzAHQAcgBhAHQAbwByAFAAYQBzAHMAdwBvAHIAZAA=</Value>
                    <PlainText>false</PlainText>
                </AdministratorPassword>
                <LocalAccounts>
                    <LocalAccount wcm:action="add">
                        <Password>
                            <Value>bwBwAGEAbQBQAGEAcwBzAHcAbwByAGQA</Value>
                            <PlainText>false</PlainText>
                        </Password>
                        <Group>administrators</Group>
                        <DisplayName>opam</DisplayName>
                        <Name>opam</Name>
                        <Description>Opam User</Description>
                    </LocalAccount>
                </LocalAccounts>
            </UserAccounts>
            <AutoLogon>
                <Enabled>true</Enabled>
                <LogonCount>3</LogonCount>
                <Username>opam</Username>
                <Password>
                    <Value>opam</Value>
                    <PlainText>true</PlainText>
                </Password>
            </AutoLogon>
            <OOBE>
                <HideEULAPage>true</HideEULAPage>
                <ProtectYourPC>1</ProtectYourPC>
                <HideLocalAccountScreen>true</HideLocalAccountScreen>
                <HideOEMRegistrationScreen>true</HideOEMRegistrationScreen>
                <HideOnlineAccountScreens>true</HideOnlineAccountScreens>
                <HideWirelessSetupInOOBE>true</HideWirelessSetupInOOBE>
            </OOBE>
            <FirstLogonCommands>
                <SynchronousCommand wcm:action="add">
                    <Order>1</Order>
                    <CommandLine>powershell.exe -NoProfile -ExecutionPolicy Bypass -Command "Uninstall-WindowsFeature -Name Windows-Defender -Restart"</CommandLine>
                    <Description>Remove Windows-Defender</Description>
                </SynchronousCommand>
                <SynchronousCommand wcm:action="add">
                    <Order>2</Order>
                    <CommandLine>cmd /C wmic useraccount where "name='Administrator'" set PasswordExpires=FALSE</CommandLine>
                    <Description>PasswordExpires=FALSE</Description>
                </SynchronousCommand>
                <SynchronousCommand wcm:action="add">
                    <Order>3</Order>
                    <CommandLine>powershell.exe -NoProfile -ExecutionPolicy Bypass -Command "Set-NetConnectionProfile -NetworkCategory Private -InputObject (Get-NetConnectionProfile)"</CommandLine>
                    <Description>NetworkLocation</Description>
                </SynchronousCommand>
                <SynchronousCommand wcm:action="add">
                    <Order>4</Order>
                    <CommandLine>winrm quickconfig -q</CommandLine>
                    <Description>Configure WinRM</Description>
                </SynchronousCommand>
                <SynchronousCommand wcm:action="add">
                    <Order>5</Order>
                    <CommandLine>winrm set winrm/config/service @{AllowUnencrypted="true"}</CommandLine>
                    <Description>Configure WinRM</Description>
                </SynchronousCommand>
                <SynchronousCommand wcm:action="add">
                    <Order>6</Order>
                    <CommandLine>winrm set winrm/config/service/auth @{Basic="true"}</CommandLine>
                    <Description>Configure WinRM</Description>
                </SynchronousCommand>
                <SynchronousCommand wcm:action="add">
                    <Order>7</Order>
                    <CommandLine>netsh advfirewall firewall set rule group="remote administration" new enable=yes</CommandLine>
                    <Description>Configure WinRM</Description>
                </SynchronousCommand>
                <SynchronousCommand wcm:action="add">
                    <Order>8</Order>
                    <CommandLine>net stop winrm</CommandLine>
                    <Description>Configure WinRM</Description>
                </SynchronousCommand>
                <SynchronousCommand wcm:action="add">
                    <Order>9</Order>
                    <CommandLine>net start winrm</CommandLine>
                    <Description>Configure WinRM</Description>
                </SynchronousCommand>
                <SynchronousCommand wcm:action="add">
                    <Order>10</Order>
                    <CommandLine>cmd /c "copy e:\setup-x86_64.exe c:\windows\setup-x86_64.exe"</CommandLine>
                    <Description>Copy cygwin executable</Description>
                </SynchronousCommand>
                <SynchronousCommand wcm:action="add">
                    <Order>11</Order>
                    <CommandLine>c:\windows\setup-x86_64.exe -q -O --site https://cygwin.mirror.constant.com --symlink-type native -P mingw64-x86_64-gcc-core,rsync,git,make,patch,unzip,pkgconf,pkg-config</CommandLine>
                    <Description>Install cygwin</Description>
                </SynchronousCommand>
                <SynchronousCommand wcm:action="add">
                    <Order>12</Order>
                    <CommandLine>setx /m PATH "c:\cygwin64\bin;c:\cygwin64\usr\x86_64-w64-mingw32\sys-root\mingw\bin;%PATH%"</CommandLine>
                    <Description>Set PATH environment variable</Description>
                </SynchronousCommand>
                <SynchronousCommand wcm:action="add">
                    <Order>13</Order>
                    <CommandLine>c:\cygwin64\bin\gawk.exe -i inplace "/(^#)|(^$)/{print;next}{$4=""noacl,""$4;print}" C:\cygwin64\etc\fstab</CommandLine>
                    <Description>Add opam-repository</Description>
                </SynchronousCommand>
                <SynchronousCommand wcm:action="add">
                    <Order>14</Order>
                    <CommandLine>setx /m OPAMCONFIRMLEVEL unsafe-yes</CommandLine>
                    <Description>Set PATH environment variable</Description>
                </SynchronousCommand>
                <SynchronousCommand wcm:action="add">
                    <Order>15</Order>
                    <CommandLine>setx /m OPAMYES 1</CommandLine>
                    <Description>Set PATH environment variable</Description>
                </SynchronousCommand>
                <SynchronousCommand wcm:action="add">
                    <Order>16</Order>
                    <CommandLine>reg add HKLM\SOFTWARE\OpenSSH /v DefaultShell /d c:\cygwin64\bin\bash.exe</CommandLine>
                    <Description>Configure WinRM</Description>
                </SynchronousCommand>
                <SynchronousCommand wcm:action="add">
                    <Order>17</Order>
                    <CommandLine>cmd /c "msiexec /q /norestart /i e:\openssh-win64.msi"</CommandLine>
                    <Description>Install OpenSSH</Description>
                </SynchronousCommand>
                <SynchronousCommand wcm:action="add">
                    <Order>18</Order>
                    <CommandLine>cmd /c "copy e:\id_ed25519.pub c:\programdata\ssh\administrators_authorized_keys"</CommandLine>
                    <Description>Install public key</Description>
                </SynchronousCommand>
                <SynchronousCommand wcm:action="add">
                    <Order>19</Order>
                    <CommandLine>cmd /c "echo AcceptENV * &gt;&gt; c:\programdata\ssh\sshd_config"</CommandLine>
                    <Description>Install public key</Description>
                </SynchronousCommand>
                <SynchronousCommand wcm:action="add">
                    <Order>20</Order>
                    <CommandLine>netsh advfirewall firewall set rule group="OpenSSH SSH Server Preview (sshd)" new profile=any enable=yes</CommandLine>
                    <Description>Configure OpenSSH</Description>
                </SynchronousCommand>
                <SynchronousCommand wcm:action="add">
                    <Order>21</Order>
                    <CommandLine>cmd /c "copy e:\opam-2.2.exe c:\cygwin64\bin\opam.exe"</CommandLine>
                    <Description>Copy opam executable</Description>
                </SynchronousCommand>
                <SynchronousCommand wcm:action="add">
                    <Order>22</Order>
                    <CommandLine>cmd /c "copy e:\opam-2.2.exe c:\cygwin64\bin\opam-2.2.exe"</CommandLine>
                    <Description>Copy opam executable</Description>
                </SynchronousCommand>
                <SynchronousCommand wcm:action="add">
                    <Order>23</Order>
                    <CommandLine>cmd /c "copy e:\opam-dev.exe c:\cygwin64\bin\opam-dev.exe"</CommandLine>
                    <Description>Copy opam executable</Description>
                </SynchronousCommand>
                <SynchronousCommand wcm:action="add">
                    <Order>24</Order>
                    <CommandLine>c:\cygwin64\bin\bash.exe --login -c "cd /cygdrive/c/Users/opam &amp;&amp; git clone https://github.com/ocaml/opam-repository"</CommandLine>
                    <Description>Add opam-repository</Description>
                </SynchronousCommand>
                <SynchronousCommand wcm:action="add">
                    <Order>25</Order>
                    <CommandLine>c:\cygwin64\bin\opam init -y -k local -a c:\users\opam\opam-repository --bare --cygwin-location=c:\cygwin64</CommandLine>
                    <Description>Opam init</Description>
                </SynchronousCommand>
                <SynchronousCommand wcm:action="add">
                    <Order>26</Order>
                    <CommandLine>c:\cygwin64\bin\opam switch create VERSION --packages=ocaml-base-compiler.VERSION</CommandLine>
                    <Description>Opam switch</Description>
                </SynchronousCommand>
                <SynchronousCommand wcm:action="add">
                    <Order>27</Order>
                    <CommandLine>c:\cygwin64\bin\opam pin add -k version ocaml-base-compiler VERSION</CommandLine>
                    <Description>Opam switch</Description>
                </SynchronousCommand>
                <SynchronousCommand wcm:action="add">
                    <Order>28</Order>
                    <CommandLine>mkdir c:\users\opam\src</CommandLine>
                    <Description>Create a src directory</Description>
                </SynchronousCommand>
                <SynchronousCommand wcm:action="add">
                    <Order>29</Order>
                    <CommandLine>shutdown /s /t 10</CommandLine>
                    <Description>Shutdown</Description>
                </SynchronousCommand>
            </FirstLogonCommands>
            <TimeZone>UTC</TimeZone>
        </component>
    </settings>
    <settings pass="offlineServicing">
        <component name="Microsoft-Windows-PartitionManager" processorArchitecture="amd64" publicKeyToken="31bf3856ad364e35" language="neutral" versionScope="nonSxS" xmlns:wcm="http://schemas.microsoft.com/WMIConfig/2002/State" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
            <SanPolicy>1</SanPolicy>
        </component>
    </settings>
</unattend>
