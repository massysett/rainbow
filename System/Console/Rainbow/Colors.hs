-- | Ordinarily you should not need this module.  Typically you will
-- just need to use "System.Console.Rainbow.ColorChunks", which is
-- re-exported from "System.Console.Rainbow".  However this module
-- can be useful if you want names for individual colors, as opposed
-- to names for chunks, which is what
-- "System.Console.Rainbow.ColorChunks" provides.
module System.Console.Rainbow.Colors where

import qualified System.Console.Terminfo as T

-- | Color for an 8-color terminal.  Nothing indicates to use the
-- default color for the terminal.  The Color should have a number
-- in the range from 0 to 7 (inclusive) but the module does nothing
-- to enforce this.  Use the names prefixed with @c256_@ if
-- you want to stay within the correc range of colors.

newtype Color8 = Color8 { unColor8 :: Maybe T.Color }
  deriving (Eq, Ord, Show)

-- | Color for an 256-color terminal.  Nothing indicates to use the
-- default color for the terminal.  The Color should have a number
-- in the range from 0 to 255 (inclusive) but the module does
-- nothing to enforce this.  Use the names prefixed with @c256_@ if
-- you want to stay within the correc range of colors.
newtype Color256 = Color256 { unColor256 :: Maybe T.Color }
  deriving (Eq, Ord, Show)

-- | Any color for an 8-color terminal can also be used in a
-- 256-color terminal.
to256 :: Color8 -> Color256
to256 = Color256 . unColor8

-- * 8 color

c8_default :: Color8
c8_default = Color8 Nothing

c8_black :: Color8
c8_black = Color8 (Just T.Black)

c8_red :: Color8
c8_red = Color8 (Just T.Red)

c8_green :: Color8
c8_green = Color8 (Just T.Green)

c8_yellow :: Color8
c8_yellow = Color8 (Just T.Yellow)

c8_blue :: Color8
c8_blue = Color8 (Just T.Blue)

c8_magenta :: Color8
c8_magenta = Color8 (Just T.Magenta)

c8_cyan :: Color8
c8_cyan = Color8 (Just T.Cyan)

c8_white :: Color8
c8_white = Color8 (Just T.White)

-- * 256 color

c256_default :: Color256
c256_default = Color256 Nothing

c256_0 :: Color256
c256_0 = Color256 (Just (T.ColorNumber 0))

c256_black :: Color256
c256_black = Color256 (Just (T.ColorNumber 0))

c256_1 :: Color256
c256_1 = Color256 (Just (T.ColorNumber 1))

c256_red :: Color256
c256_red = Color256 (Just (T.ColorNumber 1))

c256_2 :: Color256
c256_2 = Color256 (Just (T.ColorNumber 2))

c256_green :: Color256
c256_green = Color256 (Just (T.ColorNumber 2))

c256_3 :: Color256
c256_3 = Color256 (Just (T.ColorNumber 3))

c256_yellow :: Color256
c256_yellow = Color256 (Just (T.ColorNumber 3))

c256_4 :: Color256
c256_4 = Color256 (Just (T.ColorNumber 4))

c256_blue :: Color256
c256_blue = Color256 (Just (T.ColorNumber 4))

c256_5 :: Color256
c256_5 = Color256 (Just (T.ColorNumber 5))

c256_magenta :: Color256
c256_magenta = Color256 (Just (T.ColorNumber 5))

c256_6 :: Color256
c256_6 = Color256 (Just (T.ColorNumber 6))

c256_cyan :: Color256
c256_cyan = Color256 (Just (T.ColorNumber 6))

c256_7 :: Color256
c256_7 = Color256 (Just (T.ColorNumber 7))

c256_white :: Color256
c256_white = Color256 (Just (T.ColorNumber 7))

c256_8 :: Color256
c256_8 = Color256 (Just (T.ColorNumber 8))

c256_grey :: Color256
c256_grey = Color256 (Just (T.ColorNumber 8))

c256_9 :: Color256
c256_9 = Color256 (Just (T.ColorNumber 9))

c256_red_bright :: Color256
c256_red_bright = Color256 (Just (T.ColorNumber 9))

c256_10 :: Color256
c256_10 = Color256 (Just (T.ColorNumber 10))

c256_green_bright :: Color256
c256_green_bright = Color256 (Just (T.ColorNumber 10))

c256_11 :: Color256
c256_11 = Color256 (Just (T.ColorNumber 11))

c256_yellow_bright :: Color256
c256_yellow_bright = Color256 (Just (T.ColorNumber 11))

c256_12 :: Color256
c256_12 = Color256 (Just (T.ColorNumber 12))

c256_blue_bright :: Color256
c256_blue_bright = Color256 (Just (T.ColorNumber 12))

c256_13 :: Color256
c256_13 = Color256 (Just (T.ColorNumber 13))

c256_magenta_bright :: Color256
c256_magenta_bright = Color256 (Just (T.ColorNumber 13))

c256_14 :: Color256
c256_14 = Color256 (Just (T.ColorNumber 14))

c256_cyan_bright :: Color256
c256_cyan_bright = Color256 (Just (T.ColorNumber 14))

c256_15 :: Color256
c256_15 = Color256 (Just (T.ColorNumber 15))

c256_white_bright :: Color256
c256_white_bright = Color256 (Just (T.ColorNumber 15))

c256_16 :: Color256
c256_16 = Color256 (Just (T.ColorNumber 16))

c256_17 :: Color256
c256_17 = Color256 (Just (T.ColorNumber 17))

c256_18 :: Color256
c256_18 = Color256 (Just (T.ColorNumber 18))

c256_19 :: Color256
c256_19 = Color256 (Just (T.ColorNumber 19))

c256_20 :: Color256
c256_20 = Color256 (Just (T.ColorNumber 20))

c256_21 :: Color256
c256_21 = Color256 (Just (T.ColorNumber 21))

c256_22 :: Color256
c256_22 = Color256 (Just (T.ColorNumber 22))

c256_23 :: Color256
c256_23 = Color256 (Just (T.ColorNumber 23))

c256_24 :: Color256
c256_24 = Color256 (Just (T.ColorNumber 24))

c256_25 :: Color256
c256_25 = Color256 (Just (T.ColorNumber 25))

c256_26 :: Color256
c256_26 = Color256 (Just (T.ColorNumber 26))

c256_27 :: Color256
c256_27 = Color256 (Just (T.ColorNumber 27))

c256_28 :: Color256
c256_28 = Color256 (Just (T.ColorNumber 28))

c256_29 :: Color256
c256_29 = Color256 (Just (T.ColorNumber 29))

c256_30 :: Color256
c256_30 = Color256 (Just (T.ColorNumber 30))

c256_31 :: Color256
c256_31 = Color256 (Just (T.ColorNumber 31))

c256_32 :: Color256
c256_32 = Color256 (Just (T.ColorNumber 32))

c256_33 :: Color256
c256_33 = Color256 (Just (T.ColorNumber 33))

c256_34 :: Color256
c256_34 = Color256 (Just (T.ColorNumber 34))

c256_35 :: Color256
c256_35 = Color256 (Just (T.ColorNumber 35))

c256_36 :: Color256
c256_36 = Color256 (Just (T.ColorNumber 36))

c256_37 :: Color256
c256_37 = Color256 (Just (T.ColorNumber 37))

c256_38 :: Color256
c256_38 = Color256 (Just (T.ColorNumber 38))

c256_39 :: Color256
c256_39 = Color256 (Just (T.ColorNumber 39))

c256_40 :: Color256
c256_40 = Color256 (Just (T.ColorNumber 40))

c256_41 :: Color256
c256_41 = Color256 (Just (T.ColorNumber 41))

c256_42 :: Color256
c256_42 = Color256 (Just (T.ColorNumber 42))

c256_43 :: Color256
c256_43 = Color256 (Just (T.ColorNumber 43))

c256_44 :: Color256
c256_44 = Color256 (Just (T.ColorNumber 44))

c256_45 :: Color256
c256_45 = Color256 (Just (T.ColorNumber 45))

c256_46 :: Color256
c256_46 = Color256 (Just (T.ColorNumber 46))

c256_47 :: Color256
c256_47 = Color256 (Just (T.ColorNumber 47))

c256_48 :: Color256
c256_48 = Color256 (Just (T.ColorNumber 48))

c256_49 :: Color256
c256_49 = Color256 (Just (T.ColorNumber 49))

c256_50 :: Color256
c256_50 = Color256 (Just (T.ColorNumber 50))

c256_51 :: Color256
c256_51 = Color256 (Just (T.ColorNumber 51))

c256_52 :: Color256
c256_52 = Color256 (Just (T.ColorNumber 52))

c256_53 :: Color256
c256_53 = Color256 (Just (T.ColorNumber 53))

c256_54 :: Color256
c256_54 = Color256 (Just (T.ColorNumber 54))

c256_55 :: Color256
c256_55 = Color256 (Just (T.ColorNumber 55))

c256_56 :: Color256
c256_56 = Color256 (Just (T.ColorNumber 56))

c256_57 :: Color256
c256_57 = Color256 (Just (T.ColorNumber 57))

c256_58 :: Color256
c256_58 = Color256 (Just (T.ColorNumber 58))

c256_59 :: Color256
c256_59 = Color256 (Just (T.ColorNumber 59))

c256_60 :: Color256
c256_60 = Color256 (Just (T.ColorNumber 60))

c256_61 :: Color256
c256_61 = Color256 (Just (T.ColorNumber 61))

c256_62 :: Color256
c256_62 = Color256 (Just (T.ColorNumber 62))

c256_63 :: Color256
c256_63 = Color256 (Just (T.ColorNumber 63))

c256_64 :: Color256
c256_64 = Color256 (Just (T.ColorNumber 64))

c256_65 :: Color256
c256_65 = Color256 (Just (T.ColorNumber 65))

c256_66 :: Color256
c256_66 = Color256 (Just (T.ColorNumber 66))

c256_67 :: Color256
c256_67 = Color256 (Just (T.ColorNumber 67))

c256_68 :: Color256
c256_68 = Color256 (Just (T.ColorNumber 68))

c256_69 :: Color256
c256_69 = Color256 (Just (T.ColorNumber 69))

c256_70 :: Color256
c256_70 = Color256 (Just (T.ColorNumber 70))

c256_71 :: Color256
c256_71 = Color256 (Just (T.ColorNumber 71))

c256_72 :: Color256
c256_72 = Color256 (Just (T.ColorNumber 72))

c256_73 :: Color256
c256_73 = Color256 (Just (T.ColorNumber 73))

c256_74 :: Color256
c256_74 = Color256 (Just (T.ColorNumber 74))

c256_75 :: Color256
c256_75 = Color256 (Just (T.ColorNumber 75))

c256_76 :: Color256
c256_76 = Color256 (Just (T.ColorNumber 76))

c256_77 :: Color256
c256_77 = Color256 (Just (T.ColorNumber 77))

c256_78 :: Color256
c256_78 = Color256 (Just (T.ColorNumber 78))

c256_79 :: Color256
c256_79 = Color256 (Just (T.ColorNumber 79))

c256_80 :: Color256
c256_80 = Color256 (Just (T.ColorNumber 80))

c256_81 :: Color256
c256_81 = Color256 (Just (T.ColorNumber 81))

c256_82 :: Color256
c256_82 = Color256 (Just (T.ColorNumber 82))

c256_83 :: Color256
c256_83 = Color256 (Just (T.ColorNumber 83))

c256_84 :: Color256
c256_84 = Color256 (Just (T.ColorNumber 84))

c256_85 :: Color256
c256_85 = Color256 (Just (T.ColorNumber 85))

c256_86 :: Color256
c256_86 = Color256 (Just (T.ColorNumber 86))

c256_87 :: Color256
c256_87 = Color256 (Just (T.ColorNumber 87))

c256_88 :: Color256
c256_88 = Color256 (Just (T.ColorNumber 88))

c256_89 :: Color256
c256_89 = Color256 (Just (T.ColorNumber 89))

c256_90 :: Color256
c256_90 = Color256 (Just (T.ColorNumber 90))

c256_91 :: Color256
c256_91 = Color256 (Just (T.ColorNumber 91))

c256_92 :: Color256
c256_92 = Color256 (Just (T.ColorNumber 92))

c256_93 :: Color256
c256_93 = Color256 (Just (T.ColorNumber 93))

c256_94 :: Color256
c256_94 = Color256 (Just (T.ColorNumber 94))

c256_95 :: Color256
c256_95 = Color256 (Just (T.ColorNumber 95))

c256_96 :: Color256
c256_96 = Color256 (Just (T.ColorNumber 96))

c256_97 :: Color256
c256_97 = Color256 (Just (T.ColorNumber 97))

c256_98 :: Color256
c256_98 = Color256 (Just (T.ColorNumber 98))

c256_99 :: Color256
c256_99 = Color256 (Just (T.ColorNumber 99))

c256_100 :: Color256
c256_100 = Color256 (Just (T.ColorNumber 100))

c256_101 :: Color256
c256_101 = Color256 (Just (T.ColorNumber 101))

c256_102 :: Color256
c256_102 = Color256 (Just (T.ColorNumber 102))

c256_103 :: Color256
c256_103 = Color256 (Just (T.ColorNumber 103))

c256_104 :: Color256
c256_104 = Color256 (Just (T.ColorNumber 104))

c256_105 :: Color256
c256_105 = Color256 (Just (T.ColorNumber 105))

c256_106 :: Color256
c256_106 = Color256 (Just (T.ColorNumber 106))

c256_107 :: Color256
c256_107 = Color256 (Just (T.ColorNumber 107))

c256_108 :: Color256
c256_108 = Color256 (Just (T.ColorNumber 108))

c256_109 :: Color256
c256_109 = Color256 (Just (T.ColorNumber 109))

c256_110 :: Color256
c256_110 = Color256 (Just (T.ColorNumber 110))

c256_111 :: Color256
c256_111 = Color256 (Just (T.ColorNumber 111))

c256_112 :: Color256
c256_112 = Color256 (Just (T.ColorNumber 112))

c256_113 :: Color256
c256_113 = Color256 (Just (T.ColorNumber 113))

c256_114 :: Color256
c256_114 = Color256 (Just (T.ColorNumber 114))

c256_115 :: Color256
c256_115 = Color256 (Just (T.ColorNumber 115))

c256_116 :: Color256
c256_116 = Color256 (Just (T.ColorNumber 116))

c256_117 :: Color256
c256_117 = Color256 (Just (T.ColorNumber 117))

c256_118 :: Color256
c256_118 = Color256 (Just (T.ColorNumber 118))

c256_119 :: Color256
c256_119 = Color256 (Just (T.ColorNumber 119))

c256_120 :: Color256
c256_120 = Color256 (Just (T.ColorNumber 120))

c256_121 :: Color256
c256_121 = Color256 (Just (T.ColorNumber 121))

c256_122 :: Color256
c256_122 = Color256 (Just (T.ColorNumber 122))

c256_123 :: Color256
c256_123 = Color256 (Just (T.ColorNumber 123))

c256_124 :: Color256
c256_124 = Color256 (Just (T.ColorNumber 124))

c256_125 :: Color256
c256_125 = Color256 (Just (T.ColorNumber 125))

c256_126 :: Color256
c256_126 = Color256 (Just (T.ColorNumber 126))

c256_127 :: Color256
c256_127 = Color256 (Just (T.ColorNumber 127))

c256_128 :: Color256
c256_128 = Color256 (Just (T.ColorNumber 128))

c256_129 :: Color256
c256_129 = Color256 (Just (T.ColorNumber 129))

c256_130 :: Color256
c256_130 = Color256 (Just (T.ColorNumber 130))

c256_131 :: Color256
c256_131 = Color256 (Just (T.ColorNumber 131))

c256_132 :: Color256
c256_132 = Color256 (Just (T.ColorNumber 132))

c256_133 :: Color256
c256_133 = Color256 (Just (T.ColorNumber 133))

c256_134 :: Color256
c256_134 = Color256 (Just (T.ColorNumber 134))

c256_135 :: Color256
c256_135 = Color256 (Just (T.ColorNumber 135))

c256_136 :: Color256
c256_136 = Color256 (Just (T.ColorNumber 136))

c256_137 :: Color256
c256_137 = Color256 (Just (T.ColorNumber 137))

c256_138 :: Color256
c256_138 = Color256 (Just (T.ColorNumber 138))

c256_139 :: Color256
c256_139 = Color256 (Just (T.ColorNumber 139))

c256_140 :: Color256
c256_140 = Color256 (Just (T.ColorNumber 140))

c256_141 :: Color256
c256_141 = Color256 (Just (T.ColorNumber 141))

c256_142 :: Color256
c256_142 = Color256 (Just (T.ColorNumber 142))

c256_143 :: Color256
c256_143 = Color256 (Just (T.ColorNumber 143))

c256_144 :: Color256
c256_144 = Color256 (Just (T.ColorNumber 144))

c256_145 :: Color256
c256_145 = Color256 (Just (T.ColorNumber 145))

c256_146 :: Color256
c256_146 = Color256 (Just (T.ColorNumber 146))

c256_147 :: Color256
c256_147 = Color256 (Just (T.ColorNumber 147))

c256_148 :: Color256
c256_148 = Color256 (Just (T.ColorNumber 148))

c256_149 :: Color256
c256_149 = Color256 (Just (T.ColorNumber 149))

c256_150 :: Color256
c256_150 = Color256 (Just (T.ColorNumber 150))

c256_151 :: Color256
c256_151 = Color256 (Just (T.ColorNumber 151))

c256_152 :: Color256
c256_152 = Color256 (Just (T.ColorNumber 152))

c256_153 :: Color256
c256_153 = Color256 (Just (T.ColorNumber 153))

c256_154 :: Color256
c256_154 = Color256 (Just (T.ColorNumber 154))

c256_155 :: Color256
c256_155 = Color256 (Just (T.ColorNumber 155))

c256_156 :: Color256
c256_156 = Color256 (Just (T.ColorNumber 156))

c256_157 :: Color256
c256_157 = Color256 (Just (T.ColorNumber 157))

c256_158 :: Color256
c256_158 = Color256 (Just (T.ColorNumber 158))

c256_159 :: Color256
c256_159 = Color256 (Just (T.ColorNumber 159))

c256_160 :: Color256
c256_160 = Color256 (Just (T.ColorNumber 160))

c256_161 :: Color256
c256_161 = Color256 (Just (T.ColorNumber 161))

c256_162 :: Color256
c256_162 = Color256 (Just (T.ColorNumber 162))

c256_163 :: Color256
c256_163 = Color256 (Just (T.ColorNumber 163))

c256_164 :: Color256
c256_164 = Color256 (Just (T.ColorNumber 164))

c256_165 :: Color256
c256_165 = Color256 (Just (T.ColorNumber 165))

c256_166 :: Color256
c256_166 = Color256 (Just (T.ColorNumber 166))

c256_167 :: Color256
c256_167 = Color256 (Just (T.ColorNumber 167))

c256_168 :: Color256
c256_168 = Color256 (Just (T.ColorNumber 168))

c256_169 :: Color256
c256_169 = Color256 (Just (T.ColorNumber 169))

c256_170 :: Color256
c256_170 = Color256 (Just (T.ColorNumber 170))

c256_171 :: Color256
c256_171 = Color256 (Just (T.ColorNumber 171))

c256_172 :: Color256
c256_172 = Color256 (Just (T.ColorNumber 172))

c256_173 :: Color256
c256_173 = Color256 (Just (T.ColorNumber 173))

c256_174 :: Color256
c256_174 = Color256 (Just (T.ColorNumber 174))

c256_175 :: Color256
c256_175 = Color256 (Just (T.ColorNumber 175))

c256_176 :: Color256
c256_176 = Color256 (Just (T.ColorNumber 176))

c256_177 :: Color256
c256_177 = Color256 (Just (T.ColorNumber 177))

c256_178 :: Color256
c256_178 = Color256 (Just (T.ColorNumber 178))

c256_179 :: Color256
c256_179 = Color256 (Just (T.ColorNumber 179))

c256_180 :: Color256
c256_180 = Color256 (Just (T.ColorNumber 180))

c256_181 :: Color256
c256_181 = Color256 (Just (T.ColorNumber 181))

c256_182 :: Color256
c256_182 = Color256 (Just (T.ColorNumber 182))

c256_183 :: Color256
c256_183 = Color256 (Just (T.ColorNumber 183))

c256_184 :: Color256
c256_184 = Color256 (Just (T.ColorNumber 184))

c256_185 :: Color256
c256_185 = Color256 (Just (T.ColorNumber 185))

c256_186 :: Color256
c256_186 = Color256 (Just (T.ColorNumber 186))

c256_187 :: Color256
c256_187 = Color256 (Just (T.ColorNumber 187))

c256_188 :: Color256
c256_188 = Color256 (Just (T.ColorNumber 188))

c256_189 :: Color256
c256_189 = Color256 (Just (T.ColorNumber 189))

c256_190 :: Color256
c256_190 = Color256 (Just (T.ColorNumber 190))

c256_191 :: Color256
c256_191 = Color256 (Just (T.ColorNumber 191))

c256_192 :: Color256
c256_192 = Color256 (Just (T.ColorNumber 192))

c256_193 :: Color256
c256_193 = Color256 (Just (T.ColorNumber 193))

c256_194 :: Color256
c256_194 = Color256 (Just (T.ColorNumber 194))

c256_195 :: Color256
c256_195 = Color256 (Just (T.ColorNumber 195))

c256_196 :: Color256
c256_196 = Color256 (Just (T.ColorNumber 196))

c256_197 :: Color256
c256_197 = Color256 (Just (T.ColorNumber 197))

c256_198 :: Color256
c256_198 = Color256 (Just (T.ColorNumber 198))

c256_199 :: Color256
c256_199 = Color256 (Just (T.ColorNumber 199))

c256_200 :: Color256
c256_200 = Color256 (Just (T.ColorNumber 200))

c256_201 :: Color256
c256_201 = Color256 (Just (T.ColorNumber 201))

c256_202 :: Color256
c256_202 = Color256 (Just (T.ColorNumber 202))

c256_203 :: Color256
c256_203 = Color256 (Just (T.ColorNumber 203))

c256_204 :: Color256
c256_204 = Color256 (Just (T.ColorNumber 204))

c256_205 :: Color256
c256_205 = Color256 (Just (T.ColorNumber 205))

c256_206 :: Color256
c256_206 = Color256 (Just (T.ColorNumber 206))

c256_207 :: Color256
c256_207 = Color256 (Just (T.ColorNumber 207))

c256_208 :: Color256
c256_208 = Color256 (Just (T.ColorNumber 208))

c256_209 :: Color256
c256_209 = Color256 (Just (T.ColorNumber 209))

c256_210 :: Color256
c256_210 = Color256 (Just (T.ColorNumber 210))

c256_211 :: Color256
c256_211 = Color256 (Just (T.ColorNumber 211))

c256_212 :: Color256
c256_212 = Color256 (Just (T.ColorNumber 212))

c256_213 :: Color256
c256_213 = Color256 (Just (T.ColorNumber 213))

c256_214 :: Color256
c256_214 = Color256 (Just (T.ColorNumber 214))

c256_215 :: Color256
c256_215 = Color256 (Just (T.ColorNumber 215))

c256_216 :: Color256
c256_216 = Color256 (Just (T.ColorNumber 216))

c256_217 :: Color256
c256_217 = Color256 (Just (T.ColorNumber 217))

c256_218 :: Color256
c256_218 = Color256 (Just (T.ColorNumber 218))

c256_219 :: Color256
c256_219 = Color256 (Just (T.ColorNumber 219))

c256_220 :: Color256
c256_220 = Color256 (Just (T.ColorNumber 220))

c256_221 :: Color256
c256_221 = Color256 (Just (T.ColorNumber 221))

c256_222 :: Color256
c256_222 = Color256 (Just (T.ColorNumber 222))

c256_223 :: Color256
c256_223 = Color256 (Just (T.ColorNumber 223))

c256_224 :: Color256
c256_224 = Color256 (Just (T.ColorNumber 224))

c256_225 :: Color256
c256_225 = Color256 (Just (T.ColorNumber 225))

c256_226 :: Color256
c256_226 = Color256 (Just (T.ColorNumber 226))

c256_227 :: Color256
c256_227 = Color256 (Just (T.ColorNumber 227))

c256_228 :: Color256
c256_228 = Color256 (Just (T.ColorNumber 228))

c256_229 :: Color256
c256_229 = Color256 (Just (T.ColorNumber 229))

c256_230 :: Color256
c256_230 = Color256 (Just (T.ColorNumber 230))

c256_231 :: Color256
c256_231 = Color256 (Just (T.ColorNumber 231))

c256_232 :: Color256
c256_232 = Color256 (Just (T.ColorNumber 232))

c256_233 :: Color256
c256_233 = Color256 (Just (T.ColorNumber 233))

c256_234 :: Color256
c256_234 = Color256 (Just (T.ColorNumber 234))

c256_235 :: Color256
c256_235 = Color256 (Just (T.ColorNumber 235))

c256_236 :: Color256
c256_236 = Color256 (Just (T.ColorNumber 236))

c256_237 :: Color256
c256_237 = Color256 (Just (T.ColorNumber 237))

c256_238 :: Color256
c256_238 = Color256 (Just (T.ColorNumber 238))

c256_239 :: Color256
c256_239 = Color256 (Just (T.ColorNumber 239))

c256_240 :: Color256
c256_240 = Color256 (Just (T.ColorNumber 240))

c256_241 :: Color256
c256_241 = Color256 (Just (T.ColorNumber 241))

c256_242 :: Color256
c256_242 = Color256 (Just (T.ColorNumber 242))

c256_243 :: Color256
c256_243 = Color256 (Just (T.ColorNumber 243))

c256_244 :: Color256
c256_244 = Color256 (Just (T.ColorNumber 244))

c256_245 :: Color256
c256_245 = Color256 (Just (T.ColorNumber 245))

c256_246 :: Color256
c256_246 = Color256 (Just (T.ColorNumber 246))

c256_247 :: Color256
c256_247 = Color256 (Just (T.ColorNumber 247))

c256_248 :: Color256
c256_248 = Color256 (Just (T.ColorNumber 248))

c256_249 :: Color256
c256_249 = Color256 (Just (T.ColorNumber 249))

c256_250 :: Color256
c256_250 = Color256 (Just (T.ColorNumber 250))

c256_251 :: Color256
c256_251 = Color256 (Just (T.ColorNumber 251))

c256_252 :: Color256
c256_252 = Color256 (Just (T.ColorNumber 252))

c256_253 :: Color256
c256_253 = Color256 (Just (T.ColorNumber 253))

c256_254 :: Color256
c256_254 = Color256 (Just (T.ColorNumber 254))

c256_255 :: Color256
c256_255 = Color256 (Just (T.ColorNumber 255))

