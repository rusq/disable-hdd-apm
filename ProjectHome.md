# Download current version binaries for Windows #

To download the latest version and instructions, please proceed to [Downloads](Downloads.md) page.

# About #
## Description ##
This is a system utility, that might be useful on some laptops (for example ASUS Zenbook UX32VD with Hitachi HDD), when the clicking sound is heard while laptop is idle. The source of this clicking sound is head-parking mechanism of HDD, which parks drive's heads while idle to decrease the power consumption. While this was made with a good intent, for some reasons some HDD constantly park their heads, and unpark. With each such "click" the "Load-Cycle Count" parameter of S.M.A.R.T. system increases. The lifetime of the drive is about 600'000 Load Cycles (you can check yours by running HDTune Pro), and if the drive clicks too often, it might lead to an early device malfunction. The reason of this constant clicking is probably the incorrect setting of device idle time in the HDD firmware. On some drives this issue is fixed with a new HDD firmware version.

## Features ##
This version of DisAPM stays on-guard of the Load/Unload Cycle Count HDD S.M.A.R.T. parameter, checking it every 5 minutes, and when it detects the increase, it disables APM, therefore stops your drive from clicking. It runs in a system tray, you can exit it any time.

# License #
This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.