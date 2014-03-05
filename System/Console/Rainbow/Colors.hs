-- | Ordinarily you should not need this module.  Typically you will
-- just need to use "System.Console.Rainbow.ColorChunks", which is
-- re-exported from "System.Console.Rainbow".  However this module
-- can be useful if you want names for individual colors, as opposed
-- to names for chunks, which is what
-- "System.Console.Rainbow.ColorChunks" provides.
module System.Console.Rainbow.Colors where

import qualified System.Console.Terminfo as T

-- # 8 color foreground

c8_f_default :: Maybe T.Color
c8_f_default = Nothing

c8_f_black :: Maybe T.Color
c8_f_black = (Just T.Black)

c8_f_red :: Maybe T.Color
c8_f_red = (Just T.Red)

c8_f_green :: Maybe T.Color
c8_f_green = (Just T.Green)

c8_f_yellow :: Maybe T.Color
c8_f_yellow = (Just T.Yellow)

c8_f_blue :: Maybe T.Color
c8_f_blue = (Just T.Blue)

c8_f_magenta :: Maybe T.Color
c8_f_magenta = (Just T.Magenta)

c8_f_cyan :: Maybe T.Color
c8_f_cyan = (Just T.Cyan)

c8_f_white :: Maybe T.Color
c8_f_white = (Just T.White)


-- # 8 color background

c8_b_default :: Maybe T.Color
c8_b_default = Nothing

c8_b_black :: Maybe T.Color
c8_b_black = (Just T.Black)

c8_b_red :: Maybe T.Color
c8_b_red = (Just T.Red)

c8_b_green :: Maybe T.Color
c8_b_green = (Just T.Green)

c8_b_yellow :: Maybe T.Color
c8_b_yellow = (Just T.Yellow)

c8_b_blue :: Maybe T.Color
c8_b_blue = (Just T.Blue)

c8_b_magenta :: Maybe T.Color
c8_b_magenta = (Just T.Magenta)

c8_b_cyan :: Maybe T.Color
c8_b_cyan = (Just T.Cyan)

c8_b_white :: Maybe T.Color
c8_b_white = (Just T.White)

-- # 256 color foreground

c256_f_default :: Maybe T.Color
c256_f_default = Nothing


c256_f_0 :: Maybe T.Color
c256_f_0 = (Just (T.ColorNumber 0))

c256_f_black :: Maybe T.Color
c256_f_black = (Just (T.ColorNumber 0))

c256_f_1 :: Maybe T.Color
c256_f_1 = (Just (T.ColorNumber 1))

c256_f_red :: Maybe T.Color
c256_f_red = (Just (T.ColorNumber 1))

c256_f_2 :: Maybe T.Color
c256_f_2 = (Just (T.ColorNumber 2))

c256_f_green :: Maybe T.Color
c256_f_green = (Just (T.ColorNumber 2))

c256_f_3 :: Maybe T.Color
c256_f_3 = (Just (T.ColorNumber 3))

c256_f_yellow :: Maybe T.Color
c256_f_yellow = (Just (T.ColorNumber 3))

c256_f_4 :: Maybe T.Color
c256_f_4 = (Just (T.ColorNumber 4))

c256_f_blue :: Maybe T.Color
c256_f_blue = (Just (T.ColorNumber 4))

c256_f_5 :: Maybe T.Color
c256_f_5 = (Just (T.ColorNumber 5))

c256_f_magenta :: Maybe T.Color
c256_f_magenta = (Just (T.ColorNumber 5))

c256_f_6 :: Maybe T.Color
c256_f_6 = (Just (T.ColorNumber 6))

c256_f_cyan :: Maybe T.Color
c256_f_cyan = (Just (T.ColorNumber 6))

c256_f_7 :: Maybe T.Color
c256_f_7 = (Just (T.ColorNumber 7))

c256_f_white :: Maybe T.Color
c256_f_white = (Just (T.ColorNumber 7))

c256_f_8 :: Maybe T.Color
c256_f_8 = (Just (T.ColorNumber 8))

c256_f_grey :: Maybe T.Color
c256_f_grey = (Just (T.ColorNumber 8))

c256_f_9 :: Maybe T.Color
c256_f_9 = (Just (T.ColorNumber 9))

c256_f_red_bright :: Maybe T.Color
c256_f_red_bright = (Just (T.ColorNumber 9))

c256_f_10 :: Maybe T.Color
c256_f_10 = (Just (T.ColorNumber 10))

c256_f_green_bright :: Maybe T.Color
c256_f_green_bright = (Just (T.ColorNumber 10))

c256_f_11 :: Maybe T.Color
c256_f_11 = (Just (T.ColorNumber 11))

c256_f_yellow_bright :: Maybe T.Color
c256_f_yellow_bright = (Just (T.ColorNumber 11))

c256_f_12 :: Maybe T.Color
c256_f_12 = (Just (T.ColorNumber 12))

c256_f_blue_bright :: Maybe T.Color
c256_f_blue_bright = (Just (T.ColorNumber 12))

c256_f_13 :: Maybe T.Color
c256_f_13 = (Just (T.ColorNumber 13))

c256_f_magenta_bright :: Maybe T.Color
c256_f_magenta_bright = (Just (T.ColorNumber 13))

c256_f_14 :: Maybe T.Color
c256_f_14 = (Just (T.ColorNumber 14))

c256_f_cyan_bright :: Maybe T.Color
c256_f_cyan_bright = (Just (T.ColorNumber 14))

c256_f_15 :: Maybe T.Color
c256_f_15 = (Just (T.ColorNumber 15))

c256_f_white_bright :: Maybe T.Color
c256_f_white_bright = (Just (T.ColorNumber 15))

c256_f_16 :: Maybe T.Color
c256_f_16 = (Just (T.ColorNumber 16))

c256_f_17 :: Maybe T.Color
c256_f_17 = (Just (T.ColorNumber 17))

c256_f_18 :: Maybe T.Color
c256_f_18 = (Just (T.ColorNumber 18))

c256_f_19 :: Maybe T.Color
c256_f_19 = (Just (T.ColorNumber 19))

c256_f_20 :: Maybe T.Color
c256_f_20 = (Just (T.ColorNumber 20))

c256_f_21 :: Maybe T.Color
c256_f_21 = (Just (T.ColorNumber 21))

c256_f_22 :: Maybe T.Color
c256_f_22 = (Just (T.ColorNumber 22))

c256_f_23 :: Maybe T.Color
c256_f_23 = (Just (T.ColorNumber 23))

c256_f_24 :: Maybe T.Color
c256_f_24 = (Just (T.ColorNumber 24))

c256_f_25 :: Maybe T.Color
c256_f_25 = (Just (T.ColorNumber 25))

c256_f_26 :: Maybe T.Color
c256_f_26 = (Just (T.ColorNumber 26))

c256_f_27 :: Maybe T.Color
c256_f_27 = (Just (T.ColorNumber 27))

c256_f_28 :: Maybe T.Color
c256_f_28 = (Just (T.ColorNumber 28))

c256_f_29 :: Maybe T.Color
c256_f_29 = (Just (T.ColorNumber 29))

c256_f_30 :: Maybe T.Color
c256_f_30 = (Just (T.ColorNumber 30))

c256_f_31 :: Maybe T.Color
c256_f_31 = (Just (T.ColorNumber 31))

c256_f_32 :: Maybe T.Color
c256_f_32 = (Just (T.ColorNumber 32))

c256_f_33 :: Maybe T.Color
c256_f_33 = (Just (T.ColorNumber 33))

c256_f_34 :: Maybe T.Color
c256_f_34 = (Just (T.ColorNumber 34))

c256_f_35 :: Maybe T.Color
c256_f_35 = (Just (T.ColorNumber 35))

c256_f_36 :: Maybe T.Color
c256_f_36 = (Just (T.ColorNumber 36))

c256_f_37 :: Maybe T.Color
c256_f_37 = (Just (T.ColorNumber 37))

c256_f_38 :: Maybe T.Color
c256_f_38 = (Just (T.ColorNumber 38))

c256_f_39 :: Maybe T.Color
c256_f_39 = (Just (T.ColorNumber 39))

c256_f_40 :: Maybe T.Color
c256_f_40 = (Just (T.ColorNumber 40))

c256_f_41 :: Maybe T.Color
c256_f_41 = (Just (T.ColorNumber 41))

c256_f_42 :: Maybe T.Color
c256_f_42 = (Just (T.ColorNumber 42))

c256_f_43 :: Maybe T.Color
c256_f_43 = (Just (T.ColorNumber 43))

c256_f_44 :: Maybe T.Color
c256_f_44 = (Just (T.ColorNumber 44))

c256_f_45 :: Maybe T.Color
c256_f_45 = (Just (T.ColorNumber 45))

c256_f_46 :: Maybe T.Color
c256_f_46 = (Just (T.ColorNumber 46))

c256_f_47 :: Maybe T.Color
c256_f_47 = (Just (T.ColorNumber 47))

c256_f_48 :: Maybe T.Color
c256_f_48 = (Just (T.ColorNumber 48))

c256_f_49 :: Maybe T.Color
c256_f_49 = (Just (T.ColorNumber 49))

c256_f_50 :: Maybe T.Color
c256_f_50 = (Just (T.ColorNumber 50))

c256_f_51 :: Maybe T.Color
c256_f_51 = (Just (T.ColorNumber 51))

c256_f_52 :: Maybe T.Color
c256_f_52 = (Just (T.ColorNumber 52))

c256_f_53 :: Maybe T.Color
c256_f_53 = (Just (T.ColorNumber 53))

c256_f_54 :: Maybe T.Color
c256_f_54 = (Just (T.ColorNumber 54))

c256_f_55 :: Maybe T.Color
c256_f_55 = (Just (T.ColorNumber 55))

c256_f_56 :: Maybe T.Color
c256_f_56 = (Just (T.ColorNumber 56))

c256_f_57 :: Maybe T.Color
c256_f_57 = (Just (T.ColorNumber 57))

c256_f_58 :: Maybe T.Color
c256_f_58 = (Just (T.ColorNumber 58))

c256_f_59 :: Maybe T.Color
c256_f_59 = (Just (T.ColorNumber 59))

c256_f_60 :: Maybe T.Color
c256_f_60 = (Just (T.ColorNumber 60))

c256_f_61 :: Maybe T.Color
c256_f_61 = (Just (T.ColorNumber 61))

c256_f_62 :: Maybe T.Color
c256_f_62 = (Just (T.ColorNumber 62))

c256_f_63 :: Maybe T.Color
c256_f_63 = (Just (T.ColorNumber 63))

c256_f_64 :: Maybe T.Color
c256_f_64 = (Just (T.ColorNumber 64))

c256_f_65 :: Maybe T.Color
c256_f_65 = (Just (T.ColorNumber 65))

c256_f_66 :: Maybe T.Color
c256_f_66 = (Just (T.ColorNumber 66))

c256_f_67 :: Maybe T.Color
c256_f_67 = (Just (T.ColorNumber 67))

c256_f_68 :: Maybe T.Color
c256_f_68 = (Just (T.ColorNumber 68))

c256_f_69 :: Maybe T.Color
c256_f_69 = (Just (T.ColorNumber 69))

c256_f_70 :: Maybe T.Color
c256_f_70 = (Just (T.ColorNumber 70))

c256_f_71 :: Maybe T.Color
c256_f_71 = (Just (T.ColorNumber 71))

c256_f_72 :: Maybe T.Color
c256_f_72 = (Just (T.ColorNumber 72))

c256_f_73 :: Maybe T.Color
c256_f_73 = (Just (T.ColorNumber 73))

c256_f_74 :: Maybe T.Color
c256_f_74 = (Just (T.ColorNumber 74))

c256_f_75 :: Maybe T.Color
c256_f_75 = (Just (T.ColorNumber 75))

c256_f_76 :: Maybe T.Color
c256_f_76 = (Just (T.ColorNumber 76))

c256_f_77 :: Maybe T.Color
c256_f_77 = (Just (T.ColorNumber 77))

c256_f_78 :: Maybe T.Color
c256_f_78 = (Just (T.ColorNumber 78))

c256_f_79 :: Maybe T.Color
c256_f_79 = (Just (T.ColorNumber 79))

c256_f_80 :: Maybe T.Color
c256_f_80 = (Just (T.ColorNumber 80))

c256_f_81 :: Maybe T.Color
c256_f_81 = (Just (T.ColorNumber 81))

c256_f_82 :: Maybe T.Color
c256_f_82 = (Just (T.ColorNumber 82))

c256_f_83 :: Maybe T.Color
c256_f_83 = (Just (T.ColorNumber 83))

c256_f_84 :: Maybe T.Color
c256_f_84 = (Just (T.ColorNumber 84))

c256_f_85 :: Maybe T.Color
c256_f_85 = (Just (T.ColorNumber 85))

c256_f_86 :: Maybe T.Color
c256_f_86 = (Just (T.ColorNumber 86))

c256_f_87 :: Maybe T.Color
c256_f_87 = (Just (T.ColorNumber 87))

c256_f_88 :: Maybe T.Color
c256_f_88 = (Just (T.ColorNumber 88))

c256_f_89 :: Maybe T.Color
c256_f_89 = (Just (T.ColorNumber 89))

c256_f_90 :: Maybe T.Color
c256_f_90 = (Just (T.ColorNumber 90))

c256_f_91 :: Maybe T.Color
c256_f_91 = (Just (T.ColorNumber 91))

c256_f_92 :: Maybe T.Color
c256_f_92 = (Just (T.ColorNumber 92))

c256_f_93 :: Maybe T.Color
c256_f_93 = (Just (T.ColorNumber 93))

c256_f_94 :: Maybe T.Color
c256_f_94 = (Just (T.ColorNumber 94))

c256_f_95 :: Maybe T.Color
c256_f_95 = (Just (T.ColorNumber 95))

c256_f_96 :: Maybe T.Color
c256_f_96 = (Just (T.ColorNumber 96))

c256_f_97 :: Maybe T.Color
c256_f_97 = (Just (T.ColorNumber 97))

c256_f_98 :: Maybe T.Color
c256_f_98 = (Just (T.ColorNumber 98))

c256_f_99 :: Maybe T.Color
c256_f_99 = (Just (T.ColorNumber 99))

c256_f_100 :: Maybe T.Color
c256_f_100 = (Just (T.ColorNumber 100))

c256_f_101 :: Maybe T.Color
c256_f_101 = (Just (T.ColorNumber 101))

c256_f_102 :: Maybe T.Color
c256_f_102 = (Just (T.ColorNumber 102))

c256_f_103 :: Maybe T.Color
c256_f_103 = (Just (T.ColorNumber 103))

c256_f_104 :: Maybe T.Color
c256_f_104 = (Just (T.ColorNumber 104))

c256_f_105 :: Maybe T.Color
c256_f_105 = (Just (T.ColorNumber 105))

c256_f_106 :: Maybe T.Color
c256_f_106 = (Just (T.ColorNumber 106))

c256_f_107 :: Maybe T.Color
c256_f_107 = (Just (T.ColorNumber 107))

c256_f_108 :: Maybe T.Color
c256_f_108 = (Just (T.ColorNumber 108))

c256_f_109 :: Maybe T.Color
c256_f_109 = (Just (T.ColorNumber 109))

c256_f_110 :: Maybe T.Color
c256_f_110 = (Just (T.ColorNumber 110))

c256_f_111 :: Maybe T.Color
c256_f_111 = (Just (T.ColorNumber 111))

c256_f_112 :: Maybe T.Color
c256_f_112 = (Just (T.ColorNumber 112))

c256_f_113 :: Maybe T.Color
c256_f_113 = (Just (T.ColorNumber 113))

c256_f_114 :: Maybe T.Color
c256_f_114 = (Just (T.ColorNumber 114))

c256_f_115 :: Maybe T.Color
c256_f_115 = (Just (T.ColorNumber 115))

c256_f_116 :: Maybe T.Color
c256_f_116 = (Just (T.ColorNumber 116))

c256_f_117 :: Maybe T.Color
c256_f_117 = (Just (T.ColorNumber 117))

c256_f_118 :: Maybe T.Color
c256_f_118 = (Just (T.ColorNumber 118))

c256_f_119 :: Maybe T.Color
c256_f_119 = (Just (T.ColorNumber 119))

c256_f_120 :: Maybe T.Color
c256_f_120 = (Just (T.ColorNumber 120))

c256_f_121 :: Maybe T.Color
c256_f_121 = (Just (T.ColorNumber 121))

c256_f_122 :: Maybe T.Color
c256_f_122 = (Just (T.ColorNumber 122))

c256_f_123 :: Maybe T.Color
c256_f_123 = (Just (T.ColorNumber 123))

c256_f_124 :: Maybe T.Color
c256_f_124 = (Just (T.ColorNumber 124))

c256_f_125 :: Maybe T.Color
c256_f_125 = (Just (T.ColorNumber 125))

c256_f_126 :: Maybe T.Color
c256_f_126 = (Just (T.ColorNumber 126))

c256_f_127 :: Maybe T.Color
c256_f_127 = (Just (T.ColorNumber 127))

c256_f_128 :: Maybe T.Color
c256_f_128 = (Just (T.ColorNumber 128))

c256_f_129 :: Maybe T.Color
c256_f_129 = (Just (T.ColorNumber 129))

c256_f_130 :: Maybe T.Color
c256_f_130 = (Just (T.ColorNumber 130))

c256_f_131 :: Maybe T.Color
c256_f_131 = (Just (T.ColorNumber 131))

c256_f_132 :: Maybe T.Color
c256_f_132 = (Just (T.ColorNumber 132))

c256_f_133 :: Maybe T.Color
c256_f_133 = (Just (T.ColorNumber 133))

c256_f_134 :: Maybe T.Color
c256_f_134 = (Just (T.ColorNumber 134))

c256_f_135 :: Maybe T.Color
c256_f_135 = (Just (T.ColorNumber 135))

c256_f_136 :: Maybe T.Color
c256_f_136 = (Just (T.ColorNumber 136))

c256_f_137 :: Maybe T.Color
c256_f_137 = (Just (T.ColorNumber 137))

c256_f_138 :: Maybe T.Color
c256_f_138 = (Just (T.ColorNumber 138))

c256_f_139 :: Maybe T.Color
c256_f_139 = (Just (T.ColorNumber 139))

c256_f_140 :: Maybe T.Color
c256_f_140 = (Just (T.ColorNumber 140))

c256_f_141 :: Maybe T.Color
c256_f_141 = (Just (T.ColorNumber 141))

c256_f_142 :: Maybe T.Color
c256_f_142 = (Just (T.ColorNumber 142))

c256_f_143 :: Maybe T.Color
c256_f_143 = (Just (T.ColorNumber 143))

c256_f_144 :: Maybe T.Color
c256_f_144 = (Just (T.ColorNumber 144))

c256_f_145 :: Maybe T.Color
c256_f_145 = (Just (T.ColorNumber 145))

c256_f_146 :: Maybe T.Color
c256_f_146 = (Just (T.ColorNumber 146))

c256_f_147 :: Maybe T.Color
c256_f_147 = (Just (T.ColorNumber 147))

c256_f_148 :: Maybe T.Color
c256_f_148 = (Just (T.ColorNumber 148))

c256_f_149 :: Maybe T.Color
c256_f_149 = (Just (T.ColorNumber 149))

c256_f_150 :: Maybe T.Color
c256_f_150 = (Just (T.ColorNumber 150))

c256_f_151 :: Maybe T.Color
c256_f_151 = (Just (T.ColorNumber 151))

c256_f_152 :: Maybe T.Color
c256_f_152 = (Just (T.ColorNumber 152))

c256_f_153 :: Maybe T.Color
c256_f_153 = (Just (T.ColorNumber 153))

c256_f_154 :: Maybe T.Color
c256_f_154 = (Just (T.ColorNumber 154))

c256_f_155 :: Maybe T.Color
c256_f_155 = (Just (T.ColorNumber 155))

c256_f_156 :: Maybe T.Color
c256_f_156 = (Just (T.ColorNumber 156))

c256_f_157 :: Maybe T.Color
c256_f_157 = (Just (T.ColorNumber 157))

c256_f_158 :: Maybe T.Color
c256_f_158 = (Just (T.ColorNumber 158))

c256_f_159 :: Maybe T.Color
c256_f_159 = (Just (T.ColorNumber 159))

c256_f_160 :: Maybe T.Color
c256_f_160 = (Just (T.ColorNumber 160))

c256_f_161 :: Maybe T.Color
c256_f_161 = (Just (T.ColorNumber 161))

c256_f_162 :: Maybe T.Color
c256_f_162 = (Just (T.ColorNumber 162))

c256_f_163 :: Maybe T.Color
c256_f_163 = (Just (T.ColorNumber 163))

c256_f_164 :: Maybe T.Color
c256_f_164 = (Just (T.ColorNumber 164))

c256_f_165 :: Maybe T.Color
c256_f_165 = (Just (T.ColorNumber 165))

c256_f_166 :: Maybe T.Color
c256_f_166 = (Just (T.ColorNumber 166))

c256_f_167 :: Maybe T.Color
c256_f_167 = (Just (T.ColorNumber 167))

c256_f_168 :: Maybe T.Color
c256_f_168 = (Just (T.ColorNumber 168))

c256_f_169 :: Maybe T.Color
c256_f_169 = (Just (T.ColorNumber 169))

c256_f_170 :: Maybe T.Color
c256_f_170 = (Just (T.ColorNumber 170))

c256_f_171 :: Maybe T.Color
c256_f_171 = (Just (T.ColorNumber 171))

c256_f_172 :: Maybe T.Color
c256_f_172 = (Just (T.ColorNumber 172))

c256_f_173 :: Maybe T.Color
c256_f_173 = (Just (T.ColorNumber 173))

c256_f_174 :: Maybe T.Color
c256_f_174 = (Just (T.ColorNumber 174))

c256_f_175 :: Maybe T.Color
c256_f_175 = (Just (T.ColorNumber 175))

c256_f_176 :: Maybe T.Color
c256_f_176 = (Just (T.ColorNumber 176))

c256_f_177 :: Maybe T.Color
c256_f_177 = (Just (T.ColorNumber 177))

c256_f_178 :: Maybe T.Color
c256_f_178 = (Just (T.ColorNumber 178))

c256_f_179 :: Maybe T.Color
c256_f_179 = (Just (T.ColorNumber 179))

c256_f_180 :: Maybe T.Color
c256_f_180 = (Just (T.ColorNumber 180))

c256_f_181 :: Maybe T.Color
c256_f_181 = (Just (T.ColorNumber 181))

c256_f_182 :: Maybe T.Color
c256_f_182 = (Just (T.ColorNumber 182))

c256_f_183 :: Maybe T.Color
c256_f_183 = (Just (T.ColorNumber 183))

c256_f_184 :: Maybe T.Color
c256_f_184 = (Just (T.ColorNumber 184))

c256_f_185 :: Maybe T.Color
c256_f_185 = (Just (T.ColorNumber 185))

c256_f_186 :: Maybe T.Color
c256_f_186 = (Just (T.ColorNumber 186))

c256_f_187 :: Maybe T.Color
c256_f_187 = (Just (T.ColorNumber 187))

c256_f_188 :: Maybe T.Color
c256_f_188 = (Just (T.ColorNumber 188))

c256_f_189 :: Maybe T.Color
c256_f_189 = (Just (T.ColorNumber 189))

c256_f_190 :: Maybe T.Color
c256_f_190 = (Just (T.ColorNumber 190))

c256_f_191 :: Maybe T.Color
c256_f_191 = (Just (T.ColorNumber 191))

c256_f_192 :: Maybe T.Color
c256_f_192 = (Just (T.ColorNumber 192))

c256_f_193 :: Maybe T.Color
c256_f_193 = (Just (T.ColorNumber 193))

c256_f_194 :: Maybe T.Color
c256_f_194 = (Just (T.ColorNumber 194))

c256_f_195 :: Maybe T.Color
c256_f_195 = (Just (T.ColorNumber 195))

c256_f_196 :: Maybe T.Color
c256_f_196 = (Just (T.ColorNumber 196))

c256_f_197 :: Maybe T.Color
c256_f_197 = (Just (T.ColorNumber 197))

c256_f_198 :: Maybe T.Color
c256_f_198 = (Just (T.ColorNumber 198))

c256_f_199 :: Maybe T.Color
c256_f_199 = (Just (T.ColorNumber 199))

c256_f_200 :: Maybe T.Color
c256_f_200 = (Just (T.ColorNumber 200))

c256_f_201 :: Maybe T.Color
c256_f_201 = (Just (T.ColorNumber 201))

c256_f_202 :: Maybe T.Color
c256_f_202 = (Just (T.ColorNumber 202))

c256_f_203 :: Maybe T.Color
c256_f_203 = (Just (T.ColorNumber 203))

c256_f_204 :: Maybe T.Color
c256_f_204 = (Just (T.ColorNumber 204))

c256_f_205 :: Maybe T.Color
c256_f_205 = (Just (T.ColorNumber 205))

c256_f_206 :: Maybe T.Color
c256_f_206 = (Just (T.ColorNumber 206))

c256_f_207 :: Maybe T.Color
c256_f_207 = (Just (T.ColorNumber 207))

c256_f_208 :: Maybe T.Color
c256_f_208 = (Just (T.ColorNumber 208))

c256_f_209 :: Maybe T.Color
c256_f_209 = (Just (T.ColorNumber 209))

c256_f_210 :: Maybe T.Color
c256_f_210 = (Just (T.ColorNumber 210))

c256_f_211 :: Maybe T.Color
c256_f_211 = (Just (T.ColorNumber 211))

c256_f_212 :: Maybe T.Color
c256_f_212 = (Just (T.ColorNumber 212))

c256_f_213 :: Maybe T.Color
c256_f_213 = (Just (T.ColorNumber 213))

c256_f_214 :: Maybe T.Color
c256_f_214 = (Just (T.ColorNumber 214))

c256_f_215 :: Maybe T.Color
c256_f_215 = (Just (T.ColorNumber 215))

c256_f_216 :: Maybe T.Color
c256_f_216 = (Just (T.ColorNumber 216))

c256_f_217 :: Maybe T.Color
c256_f_217 = (Just (T.ColorNumber 217))

c256_f_218 :: Maybe T.Color
c256_f_218 = (Just (T.ColorNumber 218))

c256_f_219 :: Maybe T.Color
c256_f_219 = (Just (T.ColorNumber 219))

c256_f_220 :: Maybe T.Color
c256_f_220 = (Just (T.ColorNumber 220))

c256_f_221 :: Maybe T.Color
c256_f_221 = (Just (T.ColorNumber 221))

c256_f_222 :: Maybe T.Color
c256_f_222 = (Just (T.ColorNumber 222))

c256_f_223 :: Maybe T.Color
c256_f_223 = (Just (T.ColorNumber 223))

c256_f_224 :: Maybe T.Color
c256_f_224 = (Just (T.ColorNumber 224))

c256_f_225 :: Maybe T.Color
c256_f_225 = (Just (T.ColorNumber 225))

c256_f_226 :: Maybe T.Color
c256_f_226 = (Just (T.ColorNumber 226))

c256_f_227 :: Maybe T.Color
c256_f_227 = (Just (T.ColorNumber 227))

c256_f_228 :: Maybe T.Color
c256_f_228 = (Just (T.ColorNumber 228))

c256_f_229 :: Maybe T.Color
c256_f_229 = (Just (T.ColorNumber 229))

c256_f_230 :: Maybe T.Color
c256_f_230 = (Just (T.ColorNumber 230))

c256_f_231 :: Maybe T.Color
c256_f_231 = (Just (T.ColorNumber 231))

c256_f_232 :: Maybe T.Color
c256_f_232 = (Just (T.ColorNumber 232))

c256_f_233 :: Maybe T.Color
c256_f_233 = (Just (T.ColorNumber 233))

c256_f_234 :: Maybe T.Color
c256_f_234 = (Just (T.ColorNumber 234))

c256_f_235 :: Maybe T.Color
c256_f_235 = (Just (T.ColorNumber 235))

c256_f_236 :: Maybe T.Color
c256_f_236 = (Just (T.ColorNumber 236))

c256_f_237 :: Maybe T.Color
c256_f_237 = (Just (T.ColorNumber 237))

c256_f_238 :: Maybe T.Color
c256_f_238 = (Just (T.ColorNumber 238))

c256_f_239 :: Maybe T.Color
c256_f_239 = (Just (T.ColorNumber 239))

c256_f_240 :: Maybe T.Color
c256_f_240 = (Just (T.ColorNumber 240))

c256_f_241 :: Maybe T.Color
c256_f_241 = (Just (T.ColorNumber 241))

c256_f_242 :: Maybe T.Color
c256_f_242 = (Just (T.ColorNumber 242))

c256_f_243 :: Maybe T.Color
c256_f_243 = (Just (T.ColorNumber 243))

c256_f_244 :: Maybe T.Color
c256_f_244 = (Just (T.ColorNumber 244))

c256_f_245 :: Maybe T.Color
c256_f_245 = (Just (T.ColorNumber 245))

c256_f_246 :: Maybe T.Color
c256_f_246 = (Just (T.ColorNumber 246))

c256_f_247 :: Maybe T.Color
c256_f_247 = (Just (T.ColorNumber 247))

c256_f_248 :: Maybe T.Color
c256_f_248 = (Just (T.ColorNumber 248))

c256_f_249 :: Maybe T.Color
c256_f_249 = (Just (T.ColorNumber 249))

c256_f_250 :: Maybe T.Color
c256_f_250 = (Just (T.ColorNumber 250))

c256_f_251 :: Maybe T.Color
c256_f_251 = (Just (T.ColorNumber 251))

c256_f_252 :: Maybe T.Color
c256_f_252 = (Just (T.ColorNumber 252))

c256_f_253 :: Maybe T.Color
c256_f_253 = (Just (T.ColorNumber 253))

c256_f_254 :: Maybe T.Color
c256_f_254 = (Just (T.ColorNumber 254))

c256_f_255 :: Maybe T.Color
c256_f_255 = (Just (T.ColorNumber 255))


-- # 256 color background

c256_b_default :: Maybe T.Color
c256_b_default =  Nothing

c256_b_0 :: Maybe T.Color
c256_b_0 =  (Just (T.ColorNumber 0))

c256_b_black :: Maybe T.Color
c256_b_black =  (Just (T.ColorNumber 0))

c256_b_1 :: Maybe T.Color
c256_b_1 =  (Just (T.ColorNumber 1))

c256_b_red :: Maybe T.Color
c256_b_red =  (Just (T.ColorNumber 1))

c256_b_2 :: Maybe T.Color
c256_b_2 =  (Just (T.ColorNumber 2))

c256_b_green :: Maybe T.Color
c256_b_green =  (Just (T.ColorNumber 2))

c256_b_3 :: Maybe T.Color
c256_b_3 =  (Just (T.ColorNumber 3))

c256_b_yellow :: Maybe T.Color
c256_b_yellow =  (Just (T.ColorNumber 3))

c256_b_4 :: Maybe T.Color
c256_b_4 =  (Just (T.ColorNumber 4))

c256_b_blue :: Maybe T.Color
c256_b_blue =  (Just (T.ColorNumber 4))

c256_b_5 :: Maybe T.Color
c256_b_5 =  (Just (T.ColorNumber 5))

c256_b_magenta :: Maybe T.Color
c256_b_magenta =  (Just (T.ColorNumber 5))

c256_b_6 :: Maybe T.Color
c256_b_6 =  (Just (T.ColorNumber 6))

c256_b_cyan :: Maybe T.Color
c256_b_cyan =  (Just (T.ColorNumber 6))

c256_b_7 :: Maybe T.Color
c256_b_7 =  (Just (T.ColorNumber 7))

c256_b_white :: Maybe T.Color
c256_b_white =  (Just (T.ColorNumber 7))

c256_b_8 :: Maybe T.Color
c256_b_8 =  (Just (T.ColorNumber 8))

c256_b_grey :: Maybe T.Color
c256_b_grey =  (Just (T.ColorNumber 8))

c256_b_9 :: Maybe T.Color
c256_b_9 =  (Just (T.ColorNumber 9))

c256_b_red_bright :: Maybe T.Color
c256_b_red_bright =  (Just (T.ColorNumber 9))

c256_b_10 :: Maybe T.Color
c256_b_10 =  (Just (T.ColorNumber 10))

c256_b_green_bright :: Maybe T.Color
c256_b_green_bright =  (Just (T.ColorNumber 10))

c256_b_11 :: Maybe T.Color
c256_b_11 =  (Just (T.ColorNumber 11))

c256_b_yellow_bright :: Maybe T.Color
c256_b_yellow_bright =  (Just (T.ColorNumber 11))

c256_b_12 :: Maybe T.Color
c256_b_12 =  (Just (T.ColorNumber 12))

c256_b_blue_bright :: Maybe T.Color
c256_b_blue_bright =  (Just (T.ColorNumber 12))

c256_b_13 :: Maybe T.Color
c256_b_13 =  (Just (T.ColorNumber 13))

c256_b_magenta_bright :: Maybe T.Color
c256_b_magenta_bright =  (Just (T.ColorNumber 13))

c256_b_14 :: Maybe T.Color
c256_b_14 =  (Just (T.ColorNumber 14))

c256_b_cyan_bright :: Maybe T.Color
c256_b_cyan_bright =  (Just (T.ColorNumber 14))

c256_b_15 :: Maybe T.Color
c256_b_15 =  (Just (T.ColorNumber 15))

c256_b_white_bright :: Maybe T.Color
c256_b_white_bright =  (Just (T.ColorNumber 15))

c256_b_16 :: Maybe T.Color
c256_b_16 =  (Just (T.ColorNumber 16))

c256_b_17 :: Maybe T.Color
c256_b_17 =  (Just (T.ColorNumber 17))

c256_b_18 :: Maybe T.Color
c256_b_18 =  (Just (T.ColorNumber 18))

c256_b_19 :: Maybe T.Color
c256_b_19 =  (Just (T.ColorNumber 19))

c256_b_20 :: Maybe T.Color
c256_b_20 =  (Just (T.ColorNumber 20))

c256_b_21 :: Maybe T.Color
c256_b_21 =  (Just (T.ColorNumber 21))

c256_b_22 :: Maybe T.Color
c256_b_22 =  (Just (T.ColorNumber 22))

c256_b_23 :: Maybe T.Color
c256_b_23 =  (Just (T.ColorNumber 23))

c256_b_24 :: Maybe T.Color
c256_b_24 =  (Just (T.ColorNumber 24))

c256_b_25 :: Maybe T.Color
c256_b_25 =  (Just (T.ColorNumber 25))

c256_b_26 :: Maybe T.Color
c256_b_26 =  (Just (T.ColorNumber 26))

c256_b_27 :: Maybe T.Color
c256_b_27 =  (Just (T.ColorNumber 27))

c256_b_28 :: Maybe T.Color
c256_b_28 =  (Just (T.ColorNumber 28))

c256_b_29 :: Maybe T.Color
c256_b_29 =  (Just (T.ColorNumber 29))

c256_b_30 :: Maybe T.Color
c256_b_30 =  (Just (T.ColorNumber 30))

c256_b_31 :: Maybe T.Color
c256_b_31 =  (Just (T.ColorNumber 31))

c256_b_32 :: Maybe T.Color
c256_b_32 =  (Just (T.ColorNumber 32))

c256_b_33 :: Maybe T.Color
c256_b_33 =  (Just (T.ColorNumber 33))

c256_b_34 :: Maybe T.Color
c256_b_34 =  (Just (T.ColorNumber 34))

c256_b_35 :: Maybe T.Color
c256_b_35 =  (Just (T.ColorNumber 35))

c256_b_36 :: Maybe T.Color
c256_b_36 =  (Just (T.ColorNumber 36))

c256_b_37 :: Maybe T.Color
c256_b_37 =  (Just (T.ColorNumber 37))

c256_b_38 :: Maybe T.Color
c256_b_38 =  (Just (T.ColorNumber 38))

c256_b_39 :: Maybe T.Color
c256_b_39 =  (Just (T.ColorNumber 39))

c256_b_40 :: Maybe T.Color
c256_b_40 =  (Just (T.ColorNumber 40))

c256_b_41 :: Maybe T.Color
c256_b_41 =  (Just (T.ColorNumber 41))

c256_b_42 :: Maybe T.Color
c256_b_42 =  (Just (T.ColorNumber 42))

c256_b_43 :: Maybe T.Color
c256_b_43 =  (Just (T.ColorNumber 43))

c256_b_44 :: Maybe T.Color
c256_b_44 =  (Just (T.ColorNumber 44))

c256_b_45 :: Maybe T.Color
c256_b_45 =  (Just (T.ColorNumber 45))

c256_b_46 :: Maybe T.Color
c256_b_46 =  (Just (T.ColorNumber 46))

c256_b_47 :: Maybe T.Color
c256_b_47 =  (Just (T.ColorNumber 47))

c256_b_48 :: Maybe T.Color
c256_b_48 =  (Just (T.ColorNumber 48))

c256_b_49 :: Maybe T.Color
c256_b_49 =  (Just (T.ColorNumber 49))

c256_b_50 :: Maybe T.Color
c256_b_50 =  (Just (T.ColorNumber 50))

c256_b_51 :: Maybe T.Color
c256_b_51 =  (Just (T.ColorNumber 51))

c256_b_52 :: Maybe T.Color
c256_b_52 =  (Just (T.ColorNumber 52))

c256_b_53 :: Maybe T.Color
c256_b_53 =  (Just (T.ColorNumber 53))

c256_b_54 :: Maybe T.Color
c256_b_54 =  (Just (T.ColorNumber 54))

c256_b_55 :: Maybe T.Color
c256_b_55 =  (Just (T.ColorNumber 55))

c256_b_56 :: Maybe T.Color
c256_b_56 =  (Just (T.ColorNumber 56))

c256_b_57 :: Maybe T.Color
c256_b_57 =  (Just (T.ColorNumber 57))

c256_b_58 :: Maybe T.Color
c256_b_58 =  (Just (T.ColorNumber 58))

c256_b_59 :: Maybe T.Color
c256_b_59 =  (Just (T.ColorNumber 59))

c256_b_60 :: Maybe T.Color
c256_b_60 =  (Just (T.ColorNumber 60))

c256_b_61 :: Maybe T.Color
c256_b_61 =  (Just (T.ColorNumber 61))

c256_b_62 :: Maybe T.Color
c256_b_62 =  (Just (T.ColorNumber 62))

c256_b_63 :: Maybe T.Color
c256_b_63 =  (Just (T.ColorNumber 63))

c256_b_64 :: Maybe T.Color
c256_b_64 =  (Just (T.ColorNumber 64))

c256_b_65 :: Maybe T.Color
c256_b_65 =  (Just (T.ColorNumber 65))

c256_b_66 :: Maybe T.Color
c256_b_66 =  (Just (T.ColorNumber 66))

c256_b_67 :: Maybe T.Color
c256_b_67 =  (Just (T.ColorNumber 67))

c256_b_68 :: Maybe T.Color
c256_b_68 =  (Just (T.ColorNumber 68))

c256_b_69 :: Maybe T.Color
c256_b_69 =  (Just (T.ColorNumber 69))

c256_b_70 :: Maybe T.Color
c256_b_70 =  (Just (T.ColorNumber 70))

c256_b_71 :: Maybe T.Color
c256_b_71 =  (Just (T.ColorNumber 71))

c256_b_72 :: Maybe T.Color
c256_b_72 =  (Just (T.ColorNumber 72))

c256_b_73 :: Maybe T.Color
c256_b_73 =  (Just (T.ColorNumber 73))

c256_b_74 :: Maybe T.Color
c256_b_74 =  (Just (T.ColorNumber 74))

c256_b_75 :: Maybe T.Color
c256_b_75 =  (Just (T.ColorNumber 75))

c256_b_76 :: Maybe T.Color
c256_b_76 =  (Just (T.ColorNumber 76))

c256_b_77 :: Maybe T.Color
c256_b_77 =  (Just (T.ColorNumber 77))

c256_b_78 :: Maybe T.Color
c256_b_78 =  (Just (T.ColorNumber 78))

c256_b_79 :: Maybe T.Color
c256_b_79 =  (Just (T.ColorNumber 79))

c256_b_80 :: Maybe T.Color
c256_b_80 =  (Just (T.ColorNumber 80))

c256_b_81 :: Maybe T.Color
c256_b_81 =  (Just (T.ColorNumber 81))

c256_b_82 :: Maybe T.Color
c256_b_82 =  (Just (T.ColorNumber 82))

c256_b_83 :: Maybe T.Color
c256_b_83 =  (Just (T.ColorNumber 83))

c256_b_84 :: Maybe T.Color
c256_b_84 =  (Just (T.ColorNumber 84))

c256_b_85 :: Maybe T.Color
c256_b_85 =  (Just (T.ColorNumber 85))

c256_b_86 :: Maybe T.Color
c256_b_86 =  (Just (T.ColorNumber 86))

c256_b_87 :: Maybe T.Color
c256_b_87 =  (Just (T.ColorNumber 87))

c256_b_88 :: Maybe T.Color
c256_b_88 =  (Just (T.ColorNumber 88))

c256_b_89 :: Maybe T.Color
c256_b_89 =  (Just (T.ColorNumber 89))

c256_b_90 :: Maybe T.Color
c256_b_90 =  (Just (T.ColorNumber 90))

c256_b_91 :: Maybe T.Color
c256_b_91 =  (Just (T.ColorNumber 91))

c256_b_92 :: Maybe T.Color
c256_b_92 =  (Just (T.ColorNumber 92))

c256_b_93 :: Maybe T.Color
c256_b_93 =  (Just (T.ColorNumber 93))

c256_b_94 :: Maybe T.Color
c256_b_94 =  (Just (T.ColorNumber 94))

c256_b_95 :: Maybe T.Color
c256_b_95 =  (Just (T.ColorNumber 95))

c256_b_96 :: Maybe T.Color
c256_b_96 =  (Just (T.ColorNumber 96))

c256_b_97 :: Maybe T.Color
c256_b_97 =  (Just (T.ColorNumber 97))

c256_b_98 :: Maybe T.Color
c256_b_98 =  (Just (T.ColorNumber 98))

c256_b_99 :: Maybe T.Color
c256_b_99 =  (Just (T.ColorNumber 99))

c256_b_100 :: Maybe T.Color
c256_b_100 =  (Just (T.ColorNumber 100))

c256_b_101 :: Maybe T.Color
c256_b_101 =  (Just (T.ColorNumber 101))

c256_b_102 :: Maybe T.Color
c256_b_102 =  (Just (T.ColorNumber 102))

c256_b_103 :: Maybe T.Color
c256_b_103 =  (Just (T.ColorNumber 103))

c256_b_104 :: Maybe T.Color
c256_b_104 =  (Just (T.ColorNumber 104))

c256_b_105 :: Maybe T.Color
c256_b_105 =  (Just (T.ColorNumber 105))

c256_b_106 :: Maybe T.Color
c256_b_106 =  (Just (T.ColorNumber 106))

c256_b_107 :: Maybe T.Color
c256_b_107 =  (Just (T.ColorNumber 107))

c256_b_108 :: Maybe T.Color
c256_b_108 =  (Just (T.ColorNumber 108))

c256_b_109 :: Maybe T.Color
c256_b_109 =  (Just (T.ColorNumber 109))

c256_b_110 :: Maybe T.Color
c256_b_110 =  (Just (T.ColorNumber 110))

c256_b_111 :: Maybe T.Color
c256_b_111 =  (Just (T.ColorNumber 111))

c256_b_112 :: Maybe T.Color
c256_b_112 =  (Just (T.ColorNumber 112))

c256_b_113 :: Maybe T.Color
c256_b_113 =  (Just (T.ColorNumber 113))

c256_b_114 :: Maybe T.Color
c256_b_114 =  (Just (T.ColorNumber 114))

c256_b_115 :: Maybe T.Color
c256_b_115 =  (Just (T.ColorNumber 115))

c256_b_116 :: Maybe T.Color
c256_b_116 =  (Just (T.ColorNumber 116))

c256_b_117 :: Maybe T.Color
c256_b_117 =  (Just (T.ColorNumber 117))

c256_b_118 :: Maybe T.Color
c256_b_118 =  (Just (T.ColorNumber 118))

c256_b_119 :: Maybe T.Color
c256_b_119 =  (Just (T.ColorNumber 119))

c256_b_120 :: Maybe T.Color
c256_b_120 =  (Just (T.ColorNumber 120))

c256_b_121 :: Maybe T.Color
c256_b_121 =  (Just (T.ColorNumber 121))

c256_b_122 :: Maybe T.Color
c256_b_122 =  (Just (T.ColorNumber 122))

c256_b_123 :: Maybe T.Color
c256_b_123 =  (Just (T.ColorNumber 123))

c256_b_124 :: Maybe T.Color
c256_b_124 =  (Just (T.ColorNumber 124))

c256_b_125 :: Maybe T.Color
c256_b_125 =  (Just (T.ColorNumber 125))

c256_b_126 :: Maybe T.Color
c256_b_126 =  (Just (T.ColorNumber 126))

c256_b_127 :: Maybe T.Color
c256_b_127 =  (Just (T.ColorNumber 127))

c256_b_128 :: Maybe T.Color
c256_b_128 =  (Just (T.ColorNumber 128))

c256_b_129 :: Maybe T.Color
c256_b_129 =  (Just (T.ColorNumber 129))

c256_b_130 :: Maybe T.Color
c256_b_130 =  (Just (T.ColorNumber 130))

c256_b_131 :: Maybe T.Color
c256_b_131 =  (Just (T.ColorNumber 131))

c256_b_132 :: Maybe T.Color
c256_b_132 =  (Just (T.ColorNumber 132))

c256_b_133 :: Maybe T.Color
c256_b_133 =  (Just (T.ColorNumber 133))

c256_b_134 :: Maybe T.Color
c256_b_134 =  (Just (T.ColorNumber 134))

c256_b_135 :: Maybe T.Color
c256_b_135 =  (Just (T.ColorNumber 135))

c256_b_136 :: Maybe T.Color
c256_b_136 =  (Just (T.ColorNumber 136))

c256_b_137 :: Maybe T.Color
c256_b_137 =  (Just (T.ColorNumber 137))

c256_b_138 :: Maybe T.Color
c256_b_138 =  (Just (T.ColorNumber 138))

c256_b_139 :: Maybe T.Color
c256_b_139 =  (Just (T.ColorNumber 139))

c256_b_140 :: Maybe T.Color
c256_b_140 =  (Just (T.ColorNumber 140))

c256_b_141 :: Maybe T.Color
c256_b_141 =  (Just (T.ColorNumber 141))

c256_b_142 :: Maybe T.Color
c256_b_142 =  (Just (T.ColorNumber 142))

c256_b_143 :: Maybe T.Color
c256_b_143 =  (Just (T.ColorNumber 143))

c256_b_144 :: Maybe T.Color
c256_b_144 =  (Just (T.ColorNumber 144))

c256_b_145 :: Maybe T.Color
c256_b_145 =  (Just (T.ColorNumber 145))

c256_b_146 :: Maybe T.Color
c256_b_146 =  (Just (T.ColorNumber 146))

c256_b_147 :: Maybe T.Color
c256_b_147 =  (Just (T.ColorNumber 147))

c256_b_148 :: Maybe T.Color
c256_b_148 =  (Just (T.ColorNumber 148))

c256_b_149 :: Maybe T.Color
c256_b_149 =  (Just (T.ColorNumber 149))

c256_b_150 :: Maybe T.Color
c256_b_150 =  (Just (T.ColorNumber 150))

c256_b_151 :: Maybe T.Color
c256_b_151 =  (Just (T.ColorNumber 151))

c256_b_152 :: Maybe T.Color
c256_b_152 =  (Just (T.ColorNumber 152))

c256_b_153 :: Maybe T.Color
c256_b_153 =  (Just (T.ColorNumber 153))

c256_b_154 :: Maybe T.Color
c256_b_154 =  (Just (T.ColorNumber 154))

c256_b_155 :: Maybe T.Color
c256_b_155 =  (Just (T.ColorNumber 155))

c256_b_156 :: Maybe T.Color
c256_b_156 =  (Just (T.ColorNumber 156))

c256_b_157 :: Maybe T.Color
c256_b_157 =  (Just (T.ColorNumber 157))

c256_b_158 :: Maybe T.Color
c256_b_158 =  (Just (T.ColorNumber 158))

c256_b_159 :: Maybe T.Color
c256_b_159 =  (Just (T.ColorNumber 159))

c256_b_160 :: Maybe T.Color
c256_b_160 =  (Just (T.ColorNumber 160))

c256_b_161 :: Maybe T.Color
c256_b_161 =  (Just (T.ColorNumber 161))

c256_b_162 :: Maybe T.Color
c256_b_162 =  (Just (T.ColorNumber 162))

c256_b_163 :: Maybe T.Color
c256_b_163 =  (Just (T.ColorNumber 163))

c256_b_164 :: Maybe T.Color
c256_b_164 =  (Just (T.ColorNumber 164))

c256_b_165 :: Maybe T.Color
c256_b_165 =  (Just (T.ColorNumber 165))

c256_b_166 :: Maybe T.Color
c256_b_166 =  (Just (T.ColorNumber 166))

c256_b_167 :: Maybe T.Color
c256_b_167 =  (Just (T.ColorNumber 167))

c256_b_168 :: Maybe T.Color
c256_b_168 =  (Just (T.ColorNumber 168))

c256_b_169 :: Maybe T.Color
c256_b_169 =  (Just (T.ColorNumber 169))

c256_b_170 :: Maybe T.Color
c256_b_170 =  (Just (T.ColorNumber 170))

c256_b_171 :: Maybe T.Color
c256_b_171 =  (Just (T.ColorNumber 171))

c256_b_172 :: Maybe T.Color
c256_b_172 =  (Just (T.ColorNumber 172))

c256_b_173 :: Maybe T.Color
c256_b_173 =  (Just (T.ColorNumber 173))

c256_b_174 :: Maybe T.Color
c256_b_174 =  (Just (T.ColorNumber 174))

c256_b_175 :: Maybe T.Color
c256_b_175 =  (Just (T.ColorNumber 175))

c256_b_176 :: Maybe T.Color
c256_b_176 =  (Just (T.ColorNumber 176))

c256_b_177 :: Maybe T.Color
c256_b_177 =  (Just (T.ColorNumber 177))

c256_b_178 :: Maybe T.Color
c256_b_178 =  (Just (T.ColorNumber 178))

c256_b_179 :: Maybe T.Color
c256_b_179 =  (Just (T.ColorNumber 179))

c256_b_180 :: Maybe T.Color
c256_b_180 =  (Just (T.ColorNumber 180))

c256_b_181 :: Maybe T.Color
c256_b_181 =  (Just (T.ColorNumber 181))

c256_b_182 :: Maybe T.Color
c256_b_182 =  (Just (T.ColorNumber 182))

c256_b_183 :: Maybe T.Color
c256_b_183 =  (Just (T.ColorNumber 183))

c256_b_184 :: Maybe T.Color
c256_b_184 =  (Just (T.ColorNumber 184))

c256_b_185 :: Maybe T.Color
c256_b_185 =  (Just (T.ColorNumber 185))

c256_b_186 :: Maybe T.Color
c256_b_186 =  (Just (T.ColorNumber 186))

c256_b_187 :: Maybe T.Color
c256_b_187 =  (Just (T.ColorNumber 187))

c256_b_188 :: Maybe T.Color
c256_b_188 =  (Just (T.ColorNumber 188))

c256_b_189 :: Maybe T.Color
c256_b_189 =  (Just (T.ColorNumber 189))

c256_b_190 :: Maybe T.Color
c256_b_190 =  (Just (T.ColorNumber 190))

c256_b_191 :: Maybe T.Color
c256_b_191 =  (Just (T.ColorNumber 191))

c256_b_192 :: Maybe T.Color
c256_b_192 =  (Just (T.ColorNumber 192))

c256_b_193 :: Maybe T.Color
c256_b_193 =  (Just (T.ColorNumber 193))

c256_b_194 :: Maybe T.Color
c256_b_194 =  (Just (T.ColorNumber 194))

c256_b_195 :: Maybe T.Color
c256_b_195 =  (Just (T.ColorNumber 195))

c256_b_196 :: Maybe T.Color
c256_b_196 =  (Just (T.ColorNumber 196))

c256_b_197 :: Maybe T.Color
c256_b_197 =  (Just (T.ColorNumber 197))

c256_b_198 :: Maybe T.Color
c256_b_198 =  (Just (T.ColorNumber 198))

c256_b_199 :: Maybe T.Color
c256_b_199 =  (Just (T.ColorNumber 199))

c256_b_200 :: Maybe T.Color
c256_b_200 =  (Just (T.ColorNumber 200))

c256_b_201 :: Maybe T.Color
c256_b_201 =  (Just (T.ColorNumber 201))

c256_b_202 :: Maybe T.Color
c256_b_202 =  (Just (T.ColorNumber 202))

c256_b_203 :: Maybe T.Color
c256_b_203 =  (Just (T.ColorNumber 203))

c256_b_204 :: Maybe T.Color
c256_b_204 =  (Just (T.ColorNumber 204))

c256_b_205 :: Maybe T.Color
c256_b_205 =  (Just (T.ColorNumber 205))

c256_b_206 :: Maybe T.Color
c256_b_206 =  (Just (T.ColorNumber 206))

c256_b_207 :: Maybe T.Color
c256_b_207 =  (Just (T.ColorNumber 207))

c256_b_208 :: Maybe T.Color
c256_b_208 =  (Just (T.ColorNumber 208))

c256_b_209 :: Maybe T.Color
c256_b_209 =  (Just (T.ColorNumber 209))

c256_b_210 :: Maybe T.Color
c256_b_210 =  (Just (T.ColorNumber 210))

c256_b_211 :: Maybe T.Color
c256_b_211 =  (Just (T.ColorNumber 211))

c256_b_212 :: Maybe T.Color
c256_b_212 =  (Just (T.ColorNumber 212))

c256_b_213 :: Maybe T.Color
c256_b_213 =  (Just (T.ColorNumber 213))

c256_b_214 :: Maybe T.Color
c256_b_214 =  (Just (T.ColorNumber 214))

c256_b_215 :: Maybe T.Color
c256_b_215 =  (Just (T.ColorNumber 215))

c256_b_216 :: Maybe T.Color
c256_b_216 =  (Just (T.ColorNumber 216))

c256_b_217 :: Maybe T.Color
c256_b_217 =  (Just (T.ColorNumber 217))

c256_b_218 :: Maybe T.Color
c256_b_218 =  (Just (T.ColorNumber 218))

c256_b_219 :: Maybe T.Color
c256_b_219 =  (Just (T.ColorNumber 219))

c256_b_220 :: Maybe T.Color
c256_b_220 =  (Just (T.ColorNumber 220))

c256_b_221 :: Maybe T.Color
c256_b_221 =  (Just (T.ColorNumber 221))

c256_b_222 :: Maybe T.Color
c256_b_222 =  (Just (T.ColorNumber 222))

c256_b_223 :: Maybe T.Color
c256_b_223 =  (Just (T.ColorNumber 223))

c256_b_224 :: Maybe T.Color
c256_b_224 =  (Just (T.ColorNumber 224))

c256_b_225 :: Maybe T.Color
c256_b_225 =  (Just (T.ColorNumber 225))

c256_b_226 :: Maybe T.Color
c256_b_226 =  (Just (T.ColorNumber 226))

c256_b_227 :: Maybe T.Color
c256_b_227 =  (Just (T.ColorNumber 227))

c256_b_228 :: Maybe T.Color
c256_b_228 =  (Just (T.ColorNumber 228))

c256_b_229 :: Maybe T.Color
c256_b_229 =  (Just (T.ColorNumber 229))

c256_b_230 :: Maybe T.Color
c256_b_230 =  (Just (T.ColorNumber 230))

c256_b_231 :: Maybe T.Color
c256_b_231 =  (Just (T.ColorNumber 231))

c256_b_232 :: Maybe T.Color
c256_b_232 =  (Just (T.ColorNumber 232))

c256_b_233 :: Maybe T.Color
c256_b_233 =  (Just (T.ColorNumber 233))

c256_b_234 :: Maybe T.Color
c256_b_234 =  (Just (T.ColorNumber 234))

c256_b_235 :: Maybe T.Color
c256_b_235 =  (Just (T.ColorNumber 235))

c256_b_236 :: Maybe T.Color
c256_b_236 =  (Just (T.ColorNumber 236))

c256_b_237 :: Maybe T.Color
c256_b_237 =  (Just (T.ColorNumber 237))

c256_b_238 :: Maybe T.Color
c256_b_238 =  (Just (T.ColorNumber 238))

c256_b_239 :: Maybe T.Color
c256_b_239 =  (Just (T.ColorNumber 239))

c256_b_240 :: Maybe T.Color
c256_b_240 =  (Just (T.ColorNumber 240))

c256_b_241 :: Maybe T.Color
c256_b_241 =  (Just (T.ColorNumber 241))

c256_b_242 :: Maybe T.Color
c256_b_242 =  (Just (T.ColorNumber 242))

c256_b_243 :: Maybe T.Color
c256_b_243 =  (Just (T.ColorNumber 243))

c256_b_244 :: Maybe T.Color
c256_b_244 =  (Just (T.ColorNumber 244))

c256_b_245 :: Maybe T.Color
c256_b_245 =  (Just (T.ColorNumber 245))

c256_b_246 :: Maybe T.Color
c256_b_246 =  (Just (T.ColorNumber 246))

c256_b_247 :: Maybe T.Color
c256_b_247 =  (Just (T.ColorNumber 247))

c256_b_248 :: Maybe T.Color
c256_b_248 =  (Just (T.ColorNumber 248))

c256_b_249 :: Maybe T.Color
c256_b_249 =  (Just (T.ColorNumber 249))

c256_b_250 :: Maybe T.Color
c256_b_250 =  (Just (T.ColorNumber 250))

c256_b_251 :: Maybe T.Color
c256_b_251 =  (Just (T.ColorNumber 251))

c256_b_252 :: Maybe T.Color
c256_b_252 =  (Just (T.ColorNumber 252))

c256_b_253 :: Maybe T.Color
c256_b_253 =  (Just (T.ColorNumber 253))

c256_b_254 :: Maybe T.Color
c256_b_254 =  (Just (T.ColorNumber 254))

c256_b_255 :: Maybe T.Color
c256_b_255 =  (Just (T.ColorNumber 255))

