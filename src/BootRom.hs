module BootRom (loadBootRom) where

loadBootRom :: Gameboy -> IO Gameboy
loadBootRom gb = (\gb_ -> setMemory 0x00FF 0x50 gb_) .|
                 (\gb_ -> setMemory 0x00FE 0xE0 gb_) .|
                 (\gb_ -> setMemory 0x00FD 0x01 gb_) .|
                 (\gb_ -> setMemory 0x00FC 0x3E gb_) .|
                 (\gb_ -> setMemory 0x00FB 0xFE gb_) .|
                 (\gb_ -> setMemory 0x00FA 0x20 gb_) .|
                 (\gb_ -> setMemory 0x00F9 0x86 gb_) .|
                 (\gb_ -> setMemory 0x00F8 0xFB gb_) .|
                 (\gb_ -> setMemory 0x00F7 0x20 gb_) .|
                 (\gb_ -> setMemory 0x00F6 0x05 gb_) .|
                 (\gb_ -> setMemory 0x00F5 0x23 gb_) .|
                 (\gb_ -> setMemory 0x00F4 0x86 gb_) .|
                 (\gb_ -> setMemory 0x00F3 0x78 gb_) .|
                 (\gb_ -> setMemory 0x00F2 0x19 gb_) .|
                 (\gb_ -> setMemory 0x00F1 0x06 gb_) .|
                 (\gb_ -> setMemory 0x00F0 0xF5 gb_) .|
                 (\gb_ -> setMemory 0x00EF 0x20 gb_) .|
                 (\gb_ -> setMemory 0x00EE 0x34 gb_) .|
                 (\gb_ -> setMemory 0x00ED 0xFE gb_) .|
                 (\gb_ -> setMemory 0x00EC 0x7D gb_) .|
                 (\gb_ -> setMemory 0x00EB 0x23 gb_) .|
                 (\gb_ -> setMemory 0x00EA 0xFE gb_) .|
                 (\gb_ -> setMemory 0x00E9 0x20 gb_) .|
                 (\gb_ -> setMemory 0x00E8 0xBE gb_) .|
                 (\gb_ -> setMemory 0x00E7 0x13 gb_) .|
                 (\gb_ -> setMemory 0x00E6 0x1A gb_) .|
                 (\gb_ -> setMemory 0x00E5 0x00 gb_) .|
                 (\gb_ -> setMemory 0x00E4 0xA8 gb_) .|
                 (\gb_ -> setMemory 0x00E3 0x11 gb_) .|
                 (\gb_ -> setMemory 0x00E2 0x01 gb_) .|
                 (\gb_ -> setMemory 0x00E1 0x04 gb_) .|
                 (\gb_ -> setMemory 0x00E0 0x21 gb_) .|
                 (\gb_ -> setMemory 0x00DF 0x3C gb_) .|
                 (\gb_ -> setMemory 0x00DE 0x42 gb_) .|
                 (\gb_ -> setMemory 0x00DD 0xA5 gb_) .|
                 (\gb_ -> setMemory 0x00DC 0xB9 gb_) .|
                 (\gb_ -> setMemory 0x00DB 0xA5 gb_) .|
                 (\gb_ -> setMemory 0x00DA 0xB9 gb_) .|
                 (\gb_ -> setMemory 0x00D9 0x42 gb_) .|
                 (\gb_ -> setMemory 0x00D8 0x3C gb_) .|
                 (\gb_ -> setMemory 0x00D7 0x3E gb_) .|
                 (\gb_ -> setMemory 0x00D6 0x33 gb_) .|
                 (\gb_ -> setMemory 0x00D5 0xB9 gb_) .|
                 (\gb_ -> setMemory 0x00D4 0xBB gb_) .|
                 (\gb_ -> setMemory 0x00D3 0x9F gb_) .|
                 (\gb_ -> setMemory 0x00D2 0x99 gb_) .|
                 (\gb_ -> setMemory 0x00D1 0xDC gb_) .|
                 (\gb_ -> setMemory 0x00D0 0xDD gb_) .|
                 (\gb_ -> setMemory 0x00CF 0xCC gb_) .|
                 (\gb_ -> setMemory 0x00CE 0xEC gb_) .|
                 (\gb_ -> setMemory 0x00CD 0x0E gb_) .|
                 (\gb_ -> setMemory 0x00CC 0x6E gb_) .|
                 (\gb_ -> setMemory 0x00CB 0x63 gb_) .|
                 (\gb_ -> setMemory 0x00CA 0x67 gb_) .|
                 (\gb_ -> setMemory 0x00C9 0xBB gb_) .|
                 (\gb_ -> setMemory 0x00C8 0xBB gb_) .|
                 (\gb_ -> setMemory 0x00C7 0x99 gb_) .|
                 (\gb_ -> setMemory 0x00C6 0xD9 gb_) .|
                 (\gb_ -> setMemory 0x00C5 0xDD gb_) .|
                 (\gb_ -> setMemory 0x00C4 0xDD gb_) .|
                 (\gb_ -> setMemory 0x00C3 0xE6 gb_) .|
                 (\gb_ -> setMemory 0x00C2 0x6E gb_) .|
                 (\gb_ -> setMemory 0x00C1 0xCC gb_) .|
                 (\gb_ -> setMemory 0x00C0 0xDC gb_) .|
                 (\gb_ -> setMemory 0x00BF 0x0E gb_) .|
                 (\gb_ -> setMemory 0x00BE 0x00 gb_) .|
                 (\gb_ -> setMemory 0x00BD 0x89 gb_) .|
                 (\gb_ -> setMemory 0x00BC 0x88 gb_) .|
                 (\gb_ -> setMemory 0x00BB 0x1F gb_) .|
                 (\gb_ -> setMemory 0x00BA 0x11 gb_) .|
                 (\gb_ -> setMemory 0x00B9 0x08 gb_) .|
                 (\gb_ -> setMemory 0x00B8 0x00 gb_) .|
                 (\gb_ -> setMemory 0x00B7 0x0D gb_) .|
                 (\gb_ -> setMemory 0x00B6 0x00 gb_) .|
                 (\gb_ -> setMemory 0x00B5 0x0C gb_) .|
                 (\gb_ -> setMemory 0x00B4 0x00 gb_) .|
                 (\gb_ -> setMemory 0x00B3 0x83 gb_) .|
                 (\gb_ -> setMemory 0x00B2 0x00 gb_) .|
                 (\gb_ -> setMemory 0x00B1 0x73 gb_) .|
                 (\gb_ -> setMemory 0x00B0 0x03 gb_) .|
                 (\gb_ -> setMemory 0x00AF 0x0B gb_) .|
                 (\gb_ -> setMemory 0x00AE 0x00 gb_) .|
                 (\gb_ -> setMemory 0x00AD 0x0D gb_) .|
                 (\gb_ -> setMemory 0x00AC 0xCC gb_) .|
                 (\gb_ -> setMemory 0x00AB 0x66 gb_) .|
                 (\gb_ -> setMemory 0x00AA 0x66 gb_) .|
                 (\gb_ -> setMemory 0x00A9 0xED gb_) .|
                 (\gb_ -> setMemory 0x00A8 0xCE gb_) .|
                 (\gb_ -> setMemory 0x00A7 0xC9 gb_) .|
                 (\gb_ -> setMemory 0x00A6 0x23 gb_) .| --Good
                 (\gb_ -> setMemory 0x00A5 0x22 gb_) .| --Maybe Good
                 (\gb_ -> setMemory 0x00A4 0x23 gb_) .| --Good
                 (\gb_ -> setMemory 0x00A3 0x22 gb_) .| --24607 Maybe Good
                 (\gb_ -> setMemory 0x00A2 0xF5 gb_) .| --Maybe Good
                 (\gb_ -> setMemory 0x00A1 0x20 gb_) .| --Maybe Good
                 (\gb_ -> setMemory 0x00A0 0x05 gb_) .| --Good
                 (\gb_ -> setMemory 0x009F 0x17 gb_) .| --Maybe Good
                 (\gb_ -> setMemory 0x009E 0x11 gb_) .| --Maybe Good
                 (\gb_ -> setMemory 0x009D 0xCB gb_) .| --Good
                 (\gb_ -> setMemory 0x009C 0xC1 gb_) .| --Maybe Good
                 (\gb_ -> setMemory 0x009B 0x17 gb_) .| --Maybe Good
                 (\gb_ -> setMemory 0x009A 0x11 gb_) .| --24600 Maybe Good
                 (\gb_ -> setMemory 0x0099 0xCB gb_) .| --Good
                 (\gb_ -> setMemory 0x0098 0xC5 gb_) .| --Good
                 (\gb_ -> setMemory 0x0097 0x04 gb_) .| --Good
                 (\gb_ -> setMemory 0x0096 0x06 gb_) .| --Good
                 (\gb_ -> setMemory 0x0095 0x4F gb_) .| --Good
                 (\gb_ -> setMemory 0x0094 0xCB gb_) .|
                 (\gb_ -> setMemory 0x0093 0x18 gb_) .|
                 (\gb_ -> setMemory 0x0092 0x20 gb_) .|
                 (\gb_ -> setMemory 0x0091 0x16 gb_) .|
                 (\gb_ -> setMemory 0x0090 0x4F gb_) .|
                 (\gb_ -> setMemory 0x008F 0x20 gb_) .|
                 (\gb_ -> setMemory 0x008E 0x05 gb_) .|
                 (\gb_ -> setMemory 0x008D 0xD2 gb_) .|
                 (\gb_ -> setMemory 0x008C 0x20 gb_) .|
                 (\gb_ -> setMemory 0x008B 0x15 gb_) .|
                 (\gb_ -> setMemory 0x008A 0x42 gb_) .|
                 (\gb_ -> setMemory 0x0089 0xE0 gb_) .|
                 (\gb_ -> setMemory 0x0088 0x90 gb_) .|
                 (\gb_ -> setMemory 0x0087 0x42 gb_) .|
                 (\gb_ -> setMemory 0x0086 0xF0 gb_) .|
                 (\gb_ -> setMemory 0x0085 0xE2 gb_) .|
                 (\gb_ -> setMemory 0x0084 0x87 gb_) .|
                 (\gb_ -> setMemory 0x0083 0x3E gb_) .|
                 (\gb_ -> setMemory 0x0082 0x0C gb_) .|
                 (\gb_ -> setMemory 0x0081 0xE2 gb_) .|
                 (\gb_ -> setMemory 0x0080 0x7B gb_) .|
                 (\gb_ -> setMemory 0x007F 0x06 gb_) .|
                 (\gb_ -> setMemory 0x007E 0x20 gb_) .|
                 (\gb_ -> setMemory 0x007D 0x64 gb_) .|
                 (\gb_ -> setMemory 0x007C 0xFE gb_) .|
                 (\gb_ -> setMemory 0x007B 0xC1 gb_) .|
                 (\gb_ -> setMemory 0x007A 0x1E gb_) .|
                 (\gb_ -> setMemory 0x0079 0x06 gb_) .|
                 (\gb_ -> setMemory 0x0078 0x28 gb_) .|
                 (\gb_ -> setMemory 0x0077 0x62 gb_) .|
                 (\gb_ -> setMemory 0x0076 0xFE gb_) .|
                 (\gb_ -> setMemory 0x0075 0x83 gb_) .|
                 (\gb_ -> setMemory 0x0074 0x1E gb_) .|
                 (\gb_ -> setMemory 0x0073 0x7C gb_) .|
                 (\gb_ -> setMemory 0x0072 0x24 gb_) .|
                 (\gb_ -> setMemory 0x0071 0x13 gb_) .|
                 (\gb_ -> setMemory 0x0070 0x0E gb_) .|
                 (\gb_ -> setMemory 0x006F 0xF2 gb_) .|
                 (\gb_ -> setMemory 0x006E 0x20 gb_) .|
                 (\gb_ -> setMemory 0x006D 0x1D gb_) .|
                 (\gb_ -> setMemory 0x006C 0xF7 gb_) .|
                 (\gb_ -> setMemory 0x006B 0x20 gb_) .|
                 (\gb_ -> setMemory 0x006A 0x0D gb_) .|
                 (\gb_ -> setMemory 0x0069 0xFA gb_) .|
                 (\gb_ -> setMemory 0x0068 0x20 gb_) .|
                 (\gb_ -> setMemory 0x0067 0x90 gb_) .|
                 (\gb_ -> setMemory 0x0066 0xFE gb_) .|
                 (\gb_ -> setMemory 0x0065 0x44 gb_) .|
                 (\gb_ -> setMemory 0x0064 0xF0 gb_) .|
                 (\gb_ -> setMemory 0x0063 0x0C gb_) .|
                 (\gb_ -> setMemory 0x0062 0x0E gb_) .|
                 (\gb_ -> setMemory 0x0061 0x02 gb_) .|
                 (\gb_ -> setMemory 0x0060 0x1E gb_) .|
                 (\gb_ -> setMemory 0x005F 0x04 gb_) .|
                 (\gb_ -> setMemory 0x005E 0x40 gb_) .|
                 (\gb_ -> setMemory 0x005D 0xE0 gb_) .|
                 (\gb_ -> setMemory 0x005C 0x91 gb_) .|
                 (\gb_ -> setMemory 0x005B 0x3E gb_) .|
                 (\gb_ -> setMemory 0x005A 0x42 gb_) .|
                 (\gb_ -> setMemory 0x0059 0xE0 gb_) .|
                 (\gb_ -> setMemory 0x0058 0x57 gb_) .|
                 (\gb_ -> setMemory 0x0057 0x64 gb_) .|
                 (\gb_ -> setMemory 0x0056 0x3E gb_) .|
                 (\gb_ -> setMemory 0x0055 0x67 gb_) .|
                 (\gb_ -> setMemory 0x0054 0xF3 gb_) .|
                 (\gb_ -> setMemory 0x0053 0x18 gb_) .|
                 (\gb_ -> setMemory 0x0052 0x0F gb_) .|
                 (\gb_ -> setMemory 0x0051 0x2E gb_) .|
                 (\gb_ -> setMemory 0x0050 0xF9 gb_) .|
                 (\gb_ -> setMemory 0x004F 0x20 gb_) .|
                 (\gb_ -> setMemory 0x004E 0x0D gb_) .|
                 (\gb_ -> setMemory 0x004D 0x32 gb_) .|
                 (\gb_ -> setMemory 0x004C 0x08 gb_) .|
                 (\gb_ -> setMemory 0x004B 0x28 gb_) .|
                 (\gb_ -> setMemory 0x004A 0x3D gb_) .|
                 (\gb_ -> setMemory 0x0049 0x0C gb_) .|
                 (\gb_ -> setMemory 0x0048 0x0E gb_) .|
                 (\gb_ -> setMemory 0x0047 0x99 gb_) .|
                 (\gb_ -> setMemory 0x0046 0x2F gb_) .|
                 (\gb_ -> setMemory 0x0045 0x21 gb_) .|
                 (\gb_ -> setMemory 0x0044 0x99 gb_) .|
                 (\gb_ -> setMemory 0x0043 0x10 gb_) .|
                 (\gb_ -> setMemory 0x0042 0xEA gb_) .|
                 (\gb_ -> setMemory 0x0041 0x19 gb_) .|
                 (\gb_ -> setMemory 0x0040 0x3E gb_) .|
                 (\gb_ -> setMemory 0x003F 0xF9 gb_) .|
                 (\gb_ -> setMemory 0x003E 0x20 gb_) .|
                 (\gb_ -> setMemory 0x003D 0x05 gb_) .|
                 (\gb_ -> setMemory 0x003C 0x23 gb_) .|
                 (\gb_ -> setMemory 0x003B 0x22 gb_) .|
                 (\gb_ -> setMemory 0x003A 0x13 gb_) .|
                 (\gb_ -> setMemory 0x0039 0x1A gb_) .|
                 (\gb_ -> setMemory 0x0038 0x08 gb_) .|
                 (\gb_ -> setMemory 0x0037 0x06 gb_) .|
                 (\gb_ -> setMemory 0x0036 0x00 gb_) .|
                 (\gb_ -> setMemory 0x0035 0xD8 gb_) .|
                 (\gb_ -> setMemory 0x0034 0x11 gb_) .|
                 (\gb_ -> setMemory 0x0033 0xF3 gb_) .|
                 (\gb_ -> setMemory 0x0032 0x20 gb_) .|
                 (\gb_ -> setMemory 0x0031 0x34 gb_) .|
                 (\gb_ -> setMemory 0x0030 0xFE gb_) .|
                 (\gb_ -> setMemory 0x002F 0x7B gb_) .| --Good 24628
                 (\gb_ -> setMemory 0x002E 0x13 gb_) .| --Good
                 (\gb_ -> setMemory 0x002D 0x00 gb_) .| --Good
                 (\gb_ -> setMemory 0x002C 0x96 gb_) .| --Good
                 (\gb_ -> setMemory 0x002B 0xCD gb_) .| --24611 should put PC here. : Maybe Good
                 (\gb_ -> setMemory 0x002A 0x00 gb_) .| --Good
                 (\gb_ -> setMemory 0x0029 0x95 gb_) .| --Good
                 (\gb_ -> setMemory 0x0028 0xCD gb_) .| --24596 Maybe Good
                 (\gb_ -> setMemory 0x0027 0x1A gb_) .| --Good
                 (\gb_ -> setMemory 0x0026 0x80 gb_) .| --Good
                 (\gb_ -> setMemory 0x0025 0x10 gb_) .| --Good
                 (\gb_ -> setMemory 0x0024 0x21 gb_) .| --Good
                 (\gb_ -> setMemory 0x0023 0x01 gb_) .| --Good
                 (\gb_ -> setMemory 0x0022 0x04 gb_) .| --Good
                 (\gb_ -> setMemory 0x0021 0x11 gb_) .| --Good
                 (\gb_ -> setMemory 0x0020 0x47 gb_) .| --Good
                 (\gb_ -> setMemory 0x001F 0xE0 gb_) .| --Good
                 (\gb_ -> setMemory 0x001E 0xFC gb_) .| --Good
                 (\gb_ -> setMemory 0x001D 0x3E gb_) .| --Good
                 (\gb_ -> setMemory 0x001C 0x77 gb_) .| --Maybe Good 24590 steps
                 (\gb_ -> setMemory 0x001B 0x77 gb_) .| --Good
                 (\gb_ -> setMemory 0x001A 0x3E gb_) .| --Good
                 (\gb_ -> setMemory 0x0019 0x32 gb_) .| --Good
                 (\gb_ -> setMemory 0x0018 0xE2 gb_) .| --Maybe Good
                 (\gb_ -> setMemory 0x0017 0xF3 gb_) .| --Good
                 (\gb_ -> setMemory 0x0016 0x3E gb_) .| --Good
                 (\gb_ -> setMemory 0x0015 0x0C gb_) .| --Good
                 (\gb_ -> setMemory 0x0014 0xE2 gb_) .| -- 24584 steps : Maybe Good
                 (\gb_ -> setMemory 0x0013 0x32 gb_) .| --Good
                 (\gb_ -> setMemory 0x0012 0x80 gb_) .| --Good
                 (\gb_ -> setMemory 0x0011 0x3E gb_) .| --Good
                 (\gb_ -> setMemory 0x0010 0x11 gb_) .| --Good
                 (\gb_ -> setMemory 0x000F 0x0E gb_) .| --Good
                 (\gb_ -> setMemory 0x000E 0xFF gb_) .| --Good
                 (\gb_ -> setMemory 0x000D 0x26 gb_) .| --Good
                 (\gb_ -> setMemory 0x000C 0x21 gb_) .| --Good
                 (\gb_ -> setMemory 0x000B 0xFB gb_) .| --Good
                 (\gb_ -> setMemory 0x000A 0x20 gb_) .| --Must run 24578 steps to get to this opcode : Maybe Good
                 (\gb_ -> setMemory 0x0009 0x7C gb_) .| --Maybe Good
                 (\gb_ -> setMemory 0x0008 0xCB gb_) .| --Good
                 (\gb_ -> setMemory 0x0007 0x32 gb_) .| --Good
                 (\gb_ -> setMemory 0x0006 0x9F gb_) .| --Good
                 (\gb_ -> setMemory 0x0005 0xFF gb_) .| --Good
                 (\gb_ -> setMemory 0x0004 0x21 gb_) .| --Good
                 (\gb_ -> setMemory 0x0003 0xAF gb_) .| --Good
                 (\gb_ -> setMemory 0x0002 0xFF gb_) .| --Good
                 (\gb_ -> setMemory 0x0001 0xFE gb_) .| --Good
                 (\gb_ -> setMemory 0x0000 0x31 gb_) $ gb --I may have a mistake here.
