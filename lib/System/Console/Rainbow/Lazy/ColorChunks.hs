-- | Color chunks.
--
-- Names in this module follow these connventions:
--
-- * The prefix @f_@ indicates that a Chunk affects foreground
-- colors.  The prefix @b_@ indicates background colors.
--
-- * The prefix @c8_@ indicates that a Chunk affects 8-color
-- terminals only; @c256_@ indicates 256-color terminals only.  If a
-- Chunk has neither prefix, it affects both 8- and 256-color
-- terminals.
--
-- * Some colors have names.  The color names assume a palette
-- similar to the default one that xterm uses.  Other colors simply
-- have numbers.  Ultimately the interpretation of either named or
-- numbered colors is up to the user's terminal.
module System.Console.Rainbow.Lazy.ColorChunks where

import System.Console.Rainbow.Lazy.Types
import System.Console.Rainbow.Colors
import Data.Monoid

-- * Colors for both 8 and 256 color terminals

-- ** Foreground colors

f_default :: Chunk
f_default = c8_f_default <> c256_f_default

f_black :: Chunk
f_black = c8_f_black <> c256_f_black

f_red :: Chunk
f_red = c8_f_red <> c256_f_red

f_green :: Chunk
f_green = c8_f_green <> c256_f_green

f_yellow :: Chunk
f_yellow = c8_f_yellow <> c256_f_yellow

f_blue :: Chunk
f_blue = c8_f_blue <> c256_f_blue

f_magenta :: Chunk
f_magenta = c8_f_magenta <> c256_f_magenta

f_cyan :: Chunk
f_cyan = c8_f_cyan <> c256_f_cyan

f_white :: Chunk
f_white = c8_f_white <> c256_f_white

-- ** Background colors

b_default :: Chunk
b_default = c8_b_default <> c256_b_default

b_black :: Chunk
b_black = c8_b_black <> c256_b_black

b_red :: Chunk
b_red = c8_b_red <> c256_b_red

b_green :: Chunk
b_green = c8_b_green <> c256_b_green

b_yellow :: Chunk
b_yellow = c8_b_yellow <> c256_b_yellow

b_blue :: Chunk
b_blue = c8_b_blue <> c256_b_blue

b_magenta :: Chunk
b_magenta = c8_b_magenta <> c256_b_magenta

b_cyan :: Chunk
b_cyan = c8_b_cyan <> c256_b_cyan

b_white :: Chunk
b_white = c8_b_white <> c256_b_white


-- * Specific colors

-- * 8 colors, foreground, named

c8_f_default :: Chunk
c8_f_default = fc8 c8_default

c8_f_black :: Chunk
c8_f_black = fc8 c8_black

c8_f_red :: Chunk
c8_f_red = fc8 c8_red

c8_f_green :: Chunk
c8_f_green = fc8 c8_green

c8_f_yellow :: Chunk
c8_f_yellow = fc8 c8_yellow

c8_f_blue :: Chunk
c8_f_blue = fc8 c8_blue

c8_f_magenta :: Chunk
c8_f_magenta = fc8 c8_magenta

c8_f_cyan :: Chunk
c8_f_cyan = fc8 c8_cyan

c8_f_white :: Chunk
c8_f_white = fc8 c8_white

-- * 8 colors, background, named

c8_b_default :: Chunk
c8_b_default = bc8 c8_default

c8_b_black :: Chunk
c8_b_black = bc8 c8_black

c8_b_red :: Chunk
c8_b_red = bc8 c8_red

c8_b_green :: Chunk
c8_b_green = bc8 c8_green

c8_b_yellow :: Chunk
c8_b_yellow = bc8 c8_yellow

c8_b_blue :: Chunk
c8_b_blue = bc8 c8_blue

c8_b_magenta :: Chunk
c8_b_magenta = bc8 c8_magenta

c8_b_cyan :: Chunk
c8_b_cyan = bc8 c8_cyan

c8_b_white :: Chunk
c8_b_white = bc8 c8_white

-- * 8 colors, foreground, numbered

c8_f_0 :: Chunk
c8_f_0 = fc8 c8_0

c8_f_1 :: Chunk
c8_f_1 = fc8 c8_1

c8_f_2 :: Chunk
c8_f_2 = fc8 c8_2

c8_f_3 :: Chunk
c8_f_3 = fc8 c8_3

c8_f_4 :: Chunk
c8_f_4 = fc8 c8_4

c8_f_5 :: Chunk
c8_f_5 = fc8 c8_5

c8_f_6 :: Chunk
c8_f_6 = fc8 c8_6

c8_f_7 :: Chunk
c8_f_7 = fc8 c8_7

-- * 8 colors, background, numbered

c8_b_0 :: Chunk
c8_b_0 = bc8 c8_0

c8_b_1 :: Chunk
c8_b_1 = bc8 c8_1

c8_b_2 :: Chunk
c8_b_2 = bc8 c8_2

c8_b_3 :: Chunk
c8_b_3 = bc8 c8_3

c8_b_4 :: Chunk
c8_b_4 = bc8 c8_4

c8_b_5 :: Chunk
c8_b_5 = bc8 c8_5

c8_b_6 :: Chunk
c8_b_6 = bc8 c8_6

c8_b_7 :: Chunk
c8_b_7 = bc8 c8_7

-- * 256 colors, foreground, named

c256_f_default :: Chunk
c256_f_default = fc256 c256_default

c256_f_black :: Chunk
c256_f_black = fc256 c256_black

c256_f_red :: Chunk
c256_f_red = fc256 c256_red

c256_f_green :: Chunk
c256_f_green = fc256 c256_green

c256_f_yellow :: Chunk
c256_f_yellow = fc256 c256_yellow

c256_f_blue :: Chunk
c256_f_blue = fc256 c256_blue

c256_f_magenta :: Chunk
c256_f_magenta = fc256 c256_magenta

c256_f_cyan :: Chunk
c256_f_cyan = fc256 c256_cyan

c256_f_white :: Chunk
c256_f_white = fc256 c256_white

c256_f_grey :: Chunk
c256_f_grey = fc256 c256_grey

-- * 256 colors, background, named

c256_b_default :: Chunk
c256_b_default = bc256 c256_default

c256_b_black :: Chunk
c256_b_black = bc256 c256_black

c256_b_red :: Chunk
c256_b_red = bc256 c256_red

c256_b_green :: Chunk
c256_b_green = bc256 c256_green

c256_b_yellow :: Chunk
c256_b_yellow = bc256 c256_yellow

c256_b_blue :: Chunk
c256_b_blue = bc256 c256_blue

c256_b_magenta :: Chunk
c256_b_magenta = bc256 c256_magenta

c256_b_cyan :: Chunk
c256_b_cyan = bc256 c256_cyan

c256_b_white :: Chunk
c256_b_white = bc256 c256_white

c256_b_grey :: Chunk
c256_b_grey = bc256 c256_grey

-- * 256 colors, foreground, bright, named

c256_f_red_bright :: Chunk
c256_f_red_bright = fc256 c256_red_bright

c256_f_green_bright :: Chunk
c256_f_green_bright = fc256 c256_green_bright

c256_f_yellow_bright :: Chunk
c256_f_yellow_bright = fc256 c256_yellow_bright

c256_f_blue_bright :: Chunk
c256_f_blue_bright = fc256 c256_blue_bright

c256_f_magenta_bright :: Chunk
c256_f_magenta_bright = fc256 c256_magenta_bright

c256_f_cyan_bright :: Chunk
c256_f_cyan_bright = fc256 c256_cyan_bright

c256_f_white_bright :: Chunk
c256_f_white_bright = fc256 c256_white_bright

-- * 256 colors, background, bright, named

c256_b_red_bright :: Chunk
c256_b_red_bright = bc256 c256_red_bright

c256_b_green_bright :: Chunk
c256_b_green_bright = bc256 c256_green_bright

c256_b_yellow_bright :: Chunk
c256_b_yellow_bright = bc256 c256_yellow_bright

c256_b_blue_bright :: Chunk
c256_b_blue_bright = bc256 c256_blue_bright

c256_b_magenta_bright :: Chunk
c256_b_magenta_bright = bc256 c256_magenta_bright

c256_b_cyan_bright :: Chunk
c256_b_cyan_bright = bc256 c256_cyan_bright

c256_b_white_bright :: Chunk
c256_b_white_bright = bc256 c256_white_bright

-- * 256 colors, foreground, numbered

c256_f_0 :: Chunk
c256_f_0 = fc256 c256_0

c256_f_1 :: Chunk
c256_f_1 = fc256 c256_1

c256_f_2 :: Chunk
c256_f_2 = fc256 c256_2

c256_f_3 :: Chunk
c256_f_3 = fc256 c256_3

c256_f_4 :: Chunk
c256_f_4 = fc256 c256_4

c256_f_5 :: Chunk
c256_f_5 = fc256 c256_5

c256_f_6 :: Chunk
c256_f_6 = fc256 c256_6

c256_f_7 :: Chunk
c256_f_7 = fc256 c256_7

c256_f_8 :: Chunk
c256_f_8 = fc256 c256_8

c256_f_9 :: Chunk
c256_f_9 = fc256 c256_9

c256_f_10 :: Chunk
c256_f_10 = fc256 c256_10

c256_f_11 :: Chunk
c256_f_11 = fc256 c256_11

c256_f_12 :: Chunk
c256_f_12 = fc256 c256_12

c256_f_13 :: Chunk
c256_f_13 = fc256 c256_13

c256_f_14 :: Chunk
c256_f_14 = fc256 c256_14

c256_f_15 :: Chunk
c256_f_15 = fc256 c256_15

c256_f_16 :: Chunk
c256_f_16 = fc256 c256_16

c256_f_17 :: Chunk
c256_f_17 = fc256 c256_17

c256_f_18 :: Chunk
c256_f_18 = fc256 c256_18

c256_f_19 :: Chunk
c256_f_19 = fc256 c256_19

c256_f_20 :: Chunk
c256_f_20 = fc256 c256_20

c256_f_21 :: Chunk
c256_f_21 = fc256 c256_21

c256_f_22 :: Chunk
c256_f_22 = fc256 c256_22

c256_f_23 :: Chunk
c256_f_23 = fc256 c256_23

c256_f_24 :: Chunk
c256_f_24 = fc256 c256_24

c256_f_25 :: Chunk
c256_f_25 = fc256 c256_25

c256_f_26 :: Chunk
c256_f_26 = fc256 c256_26

c256_f_27 :: Chunk
c256_f_27 = fc256 c256_27

c256_f_28 :: Chunk
c256_f_28 = fc256 c256_28

c256_f_29 :: Chunk
c256_f_29 = fc256 c256_29

c256_f_30 :: Chunk
c256_f_30 = fc256 c256_30

c256_f_31 :: Chunk
c256_f_31 = fc256 c256_31

c256_f_32 :: Chunk
c256_f_32 = fc256 c256_32

c256_f_33 :: Chunk
c256_f_33 = fc256 c256_33

c256_f_34 :: Chunk
c256_f_34 = fc256 c256_34

c256_f_35 :: Chunk
c256_f_35 = fc256 c256_35

c256_f_36 :: Chunk
c256_f_36 = fc256 c256_36

c256_f_37 :: Chunk
c256_f_37 = fc256 c256_37

c256_f_38 :: Chunk
c256_f_38 = fc256 c256_38

c256_f_39 :: Chunk
c256_f_39 = fc256 c256_39

c256_f_40 :: Chunk
c256_f_40 = fc256 c256_40

c256_f_41 :: Chunk
c256_f_41 = fc256 c256_41

c256_f_42 :: Chunk
c256_f_42 = fc256 c256_42

c256_f_43 :: Chunk
c256_f_43 = fc256 c256_43

c256_f_44 :: Chunk
c256_f_44 = fc256 c256_44

c256_f_45 :: Chunk
c256_f_45 = fc256 c256_45

c256_f_46 :: Chunk
c256_f_46 = fc256 c256_46

c256_f_47 :: Chunk
c256_f_47 = fc256 c256_47

c256_f_48 :: Chunk
c256_f_48 = fc256 c256_48

c256_f_49 :: Chunk
c256_f_49 = fc256 c256_49

c256_f_50 :: Chunk
c256_f_50 = fc256 c256_50

c256_f_51 :: Chunk
c256_f_51 = fc256 c256_51

c256_f_52 :: Chunk
c256_f_52 = fc256 c256_52

c256_f_53 :: Chunk
c256_f_53 = fc256 c256_53

c256_f_54 :: Chunk
c256_f_54 = fc256 c256_54

c256_f_55 :: Chunk
c256_f_55 = fc256 c256_55

c256_f_56 :: Chunk
c256_f_56 = fc256 c256_56

c256_f_57 :: Chunk
c256_f_57 = fc256 c256_57

c256_f_58 :: Chunk
c256_f_58 = fc256 c256_58

c256_f_59 :: Chunk
c256_f_59 = fc256 c256_59

c256_f_60 :: Chunk
c256_f_60 = fc256 c256_60

c256_f_61 :: Chunk
c256_f_61 = fc256 c256_61

c256_f_62 :: Chunk
c256_f_62 = fc256 c256_62

c256_f_63 :: Chunk
c256_f_63 = fc256 c256_63

c256_f_64 :: Chunk
c256_f_64 = fc256 c256_64

c256_f_65 :: Chunk
c256_f_65 = fc256 c256_65

c256_f_66 :: Chunk
c256_f_66 = fc256 c256_66

c256_f_67 :: Chunk
c256_f_67 = fc256 c256_67

c256_f_68 :: Chunk
c256_f_68 = fc256 c256_68

c256_f_69 :: Chunk
c256_f_69 = fc256 c256_69

c256_f_70 :: Chunk
c256_f_70 = fc256 c256_70

c256_f_71 :: Chunk
c256_f_71 = fc256 c256_71

c256_f_72 :: Chunk
c256_f_72 = fc256 c256_72

c256_f_73 :: Chunk
c256_f_73 = fc256 c256_73

c256_f_74 :: Chunk
c256_f_74 = fc256 c256_74

c256_f_75 :: Chunk
c256_f_75 = fc256 c256_75

c256_f_76 :: Chunk
c256_f_76 = fc256 c256_76

c256_f_77 :: Chunk
c256_f_77 = fc256 c256_77

c256_f_78 :: Chunk
c256_f_78 = fc256 c256_78

c256_f_79 :: Chunk
c256_f_79 = fc256 c256_79

c256_f_80 :: Chunk
c256_f_80 = fc256 c256_80

c256_f_81 :: Chunk
c256_f_81 = fc256 c256_81

c256_f_82 :: Chunk
c256_f_82 = fc256 c256_82

c256_f_83 :: Chunk
c256_f_83 = fc256 c256_83

c256_f_84 :: Chunk
c256_f_84 = fc256 c256_84

c256_f_85 :: Chunk
c256_f_85 = fc256 c256_85

c256_f_86 :: Chunk
c256_f_86 = fc256 c256_86

c256_f_87 :: Chunk
c256_f_87 = fc256 c256_87

c256_f_88 :: Chunk
c256_f_88 = fc256 c256_88

c256_f_89 :: Chunk
c256_f_89 = fc256 c256_89

c256_f_90 :: Chunk
c256_f_90 = fc256 c256_90

c256_f_91 :: Chunk
c256_f_91 = fc256 c256_91

c256_f_92 :: Chunk
c256_f_92 = fc256 c256_92

c256_f_93 :: Chunk
c256_f_93 = fc256 c256_93

c256_f_94 :: Chunk
c256_f_94 = fc256 c256_94

c256_f_95 :: Chunk
c256_f_95 = fc256 c256_95

c256_f_96 :: Chunk
c256_f_96 = fc256 c256_96

c256_f_97 :: Chunk
c256_f_97 = fc256 c256_97

c256_f_98 :: Chunk
c256_f_98 = fc256 c256_98

c256_f_99 :: Chunk
c256_f_99 = fc256 c256_99

c256_f_100 :: Chunk
c256_f_100 = fc256 c256_100

c256_f_101 :: Chunk
c256_f_101 = fc256 c256_101

c256_f_102 :: Chunk
c256_f_102 = fc256 c256_102

c256_f_103 :: Chunk
c256_f_103 = fc256 c256_103

c256_f_104 :: Chunk
c256_f_104 = fc256 c256_104

c256_f_105 :: Chunk
c256_f_105 = fc256 c256_105

c256_f_106 :: Chunk
c256_f_106 = fc256 c256_106

c256_f_107 :: Chunk
c256_f_107 = fc256 c256_107

c256_f_108 :: Chunk
c256_f_108 = fc256 c256_108

c256_f_109 :: Chunk
c256_f_109 = fc256 c256_109

c256_f_110 :: Chunk
c256_f_110 = fc256 c256_110

c256_f_111 :: Chunk
c256_f_111 = fc256 c256_111

c256_f_112 :: Chunk
c256_f_112 = fc256 c256_112

c256_f_113 :: Chunk
c256_f_113 = fc256 c256_113

c256_f_114 :: Chunk
c256_f_114 = fc256 c256_114

c256_f_115 :: Chunk
c256_f_115 = fc256 c256_115

c256_f_116 :: Chunk
c256_f_116 = fc256 c256_116

c256_f_117 :: Chunk
c256_f_117 = fc256 c256_117

c256_f_118 :: Chunk
c256_f_118 = fc256 c256_118

c256_f_119 :: Chunk
c256_f_119 = fc256 c256_119

c256_f_120 :: Chunk
c256_f_120 = fc256 c256_120

c256_f_121 :: Chunk
c256_f_121 = fc256 c256_121

c256_f_122 :: Chunk
c256_f_122 = fc256 c256_122

c256_f_123 :: Chunk
c256_f_123 = fc256 c256_123

c256_f_124 :: Chunk
c256_f_124 = fc256 c256_124

c256_f_125 :: Chunk
c256_f_125 = fc256 c256_125

c256_f_126 :: Chunk
c256_f_126 = fc256 c256_126

c256_f_127 :: Chunk
c256_f_127 = fc256 c256_127

c256_f_128 :: Chunk
c256_f_128 = fc256 c256_128

c256_f_129 :: Chunk
c256_f_129 = fc256 c256_129

c256_f_130 :: Chunk
c256_f_130 = fc256 c256_130

c256_f_131 :: Chunk
c256_f_131 = fc256 c256_131

c256_f_132 :: Chunk
c256_f_132 = fc256 c256_132

c256_f_133 :: Chunk
c256_f_133 = fc256 c256_133

c256_f_134 :: Chunk
c256_f_134 = fc256 c256_134

c256_f_135 :: Chunk
c256_f_135 = fc256 c256_135

c256_f_136 :: Chunk
c256_f_136 = fc256 c256_136

c256_f_137 :: Chunk
c256_f_137 = fc256 c256_137

c256_f_138 :: Chunk
c256_f_138 = fc256 c256_138

c256_f_139 :: Chunk
c256_f_139 = fc256 c256_139

c256_f_140 :: Chunk
c256_f_140 = fc256 c256_140

c256_f_141 :: Chunk
c256_f_141 = fc256 c256_141

c256_f_142 :: Chunk
c256_f_142 = fc256 c256_142

c256_f_143 :: Chunk
c256_f_143 = fc256 c256_143

c256_f_144 :: Chunk
c256_f_144 = fc256 c256_144

c256_f_145 :: Chunk
c256_f_145 = fc256 c256_145

c256_f_146 :: Chunk
c256_f_146 = fc256 c256_146

c256_f_147 :: Chunk
c256_f_147 = fc256 c256_147

c256_f_148 :: Chunk
c256_f_148 = fc256 c256_148

c256_f_149 :: Chunk
c256_f_149 = fc256 c256_149

c256_f_150 :: Chunk
c256_f_150 = fc256 c256_150

c256_f_151 :: Chunk
c256_f_151 = fc256 c256_151

c256_f_152 :: Chunk
c256_f_152 = fc256 c256_152

c256_f_153 :: Chunk
c256_f_153 = fc256 c256_153

c256_f_154 :: Chunk
c256_f_154 = fc256 c256_154

c256_f_155 :: Chunk
c256_f_155 = fc256 c256_155

c256_f_156 :: Chunk
c256_f_156 = fc256 c256_156

c256_f_157 :: Chunk
c256_f_157 = fc256 c256_157

c256_f_158 :: Chunk
c256_f_158 = fc256 c256_158

c256_f_159 :: Chunk
c256_f_159 = fc256 c256_159

c256_f_160 :: Chunk
c256_f_160 = fc256 c256_160

c256_f_161 :: Chunk
c256_f_161 = fc256 c256_161

c256_f_162 :: Chunk
c256_f_162 = fc256 c256_162

c256_f_163 :: Chunk
c256_f_163 = fc256 c256_163

c256_f_164 :: Chunk
c256_f_164 = fc256 c256_164

c256_f_165 :: Chunk
c256_f_165 = fc256 c256_165

c256_f_166 :: Chunk
c256_f_166 = fc256 c256_166

c256_f_167 :: Chunk
c256_f_167 = fc256 c256_167

c256_f_168 :: Chunk
c256_f_168 = fc256 c256_168

c256_f_169 :: Chunk
c256_f_169 = fc256 c256_169

c256_f_170 :: Chunk
c256_f_170 = fc256 c256_170

c256_f_171 :: Chunk
c256_f_171 = fc256 c256_171

c256_f_172 :: Chunk
c256_f_172 = fc256 c256_172

c256_f_173 :: Chunk
c256_f_173 = fc256 c256_173

c256_f_174 :: Chunk
c256_f_174 = fc256 c256_174

c256_f_175 :: Chunk
c256_f_175 = fc256 c256_175

c256_f_176 :: Chunk
c256_f_176 = fc256 c256_176

c256_f_177 :: Chunk
c256_f_177 = fc256 c256_177

c256_f_178 :: Chunk
c256_f_178 = fc256 c256_178

c256_f_179 :: Chunk
c256_f_179 = fc256 c256_179

c256_f_180 :: Chunk
c256_f_180 = fc256 c256_180

c256_f_181 :: Chunk
c256_f_181 = fc256 c256_181

c256_f_182 :: Chunk
c256_f_182 = fc256 c256_182

c256_f_183 :: Chunk
c256_f_183 = fc256 c256_183

c256_f_184 :: Chunk
c256_f_184 = fc256 c256_184

c256_f_185 :: Chunk
c256_f_185 = fc256 c256_185

c256_f_186 :: Chunk
c256_f_186 = fc256 c256_186

c256_f_187 :: Chunk
c256_f_187 = fc256 c256_187

c256_f_188 :: Chunk
c256_f_188 = fc256 c256_188

c256_f_189 :: Chunk
c256_f_189 = fc256 c256_189

c256_f_190 :: Chunk
c256_f_190 = fc256 c256_190

c256_f_191 :: Chunk
c256_f_191 = fc256 c256_191

c256_f_192 :: Chunk
c256_f_192 = fc256 c256_192

c256_f_193 :: Chunk
c256_f_193 = fc256 c256_193

c256_f_194 :: Chunk
c256_f_194 = fc256 c256_194

c256_f_195 :: Chunk
c256_f_195 = fc256 c256_195

c256_f_196 :: Chunk
c256_f_196 = fc256 c256_196

c256_f_197 :: Chunk
c256_f_197 = fc256 c256_197

c256_f_198 :: Chunk
c256_f_198 = fc256 c256_198

c256_f_199 :: Chunk
c256_f_199 = fc256 c256_199

c256_f_200 :: Chunk
c256_f_200 = fc256 c256_200

c256_f_201 :: Chunk
c256_f_201 = fc256 c256_201

c256_f_202 :: Chunk
c256_f_202 = fc256 c256_202

c256_f_203 :: Chunk
c256_f_203 = fc256 c256_203

c256_f_204 :: Chunk
c256_f_204 = fc256 c256_204

c256_f_205 :: Chunk
c256_f_205 = fc256 c256_205

c256_f_206 :: Chunk
c256_f_206 = fc256 c256_206

c256_f_207 :: Chunk
c256_f_207 = fc256 c256_207

c256_f_208 :: Chunk
c256_f_208 = fc256 c256_208

c256_f_209 :: Chunk
c256_f_209 = fc256 c256_209

c256_f_210 :: Chunk
c256_f_210 = fc256 c256_210

c256_f_211 :: Chunk
c256_f_211 = fc256 c256_211

c256_f_212 :: Chunk
c256_f_212 = fc256 c256_212

c256_f_213 :: Chunk
c256_f_213 = fc256 c256_213

c256_f_214 :: Chunk
c256_f_214 = fc256 c256_214

c256_f_215 :: Chunk
c256_f_215 = fc256 c256_215

c256_f_216 :: Chunk
c256_f_216 = fc256 c256_216

c256_f_217 :: Chunk
c256_f_217 = fc256 c256_217

c256_f_218 :: Chunk
c256_f_218 = fc256 c256_218

c256_f_219 :: Chunk
c256_f_219 = fc256 c256_219

c256_f_220 :: Chunk
c256_f_220 = fc256 c256_220

c256_f_221 :: Chunk
c256_f_221 = fc256 c256_221

c256_f_222 :: Chunk
c256_f_222 = fc256 c256_222

c256_f_223 :: Chunk
c256_f_223 = fc256 c256_223

c256_f_224 :: Chunk
c256_f_224 = fc256 c256_224

c256_f_225 :: Chunk
c256_f_225 = fc256 c256_225

c256_f_226 :: Chunk
c256_f_226 = fc256 c256_226

c256_f_227 :: Chunk
c256_f_227 = fc256 c256_227

c256_f_228 :: Chunk
c256_f_228 = fc256 c256_228

c256_f_229 :: Chunk
c256_f_229 = fc256 c256_229

c256_f_230 :: Chunk
c256_f_230 = fc256 c256_230

c256_f_231 :: Chunk
c256_f_231 = fc256 c256_231

c256_f_232 :: Chunk
c256_f_232 = fc256 c256_232

c256_f_233 :: Chunk
c256_f_233 = fc256 c256_233

c256_f_234 :: Chunk
c256_f_234 = fc256 c256_234

c256_f_235 :: Chunk
c256_f_235 = fc256 c256_235

c256_f_236 :: Chunk
c256_f_236 = fc256 c256_236

c256_f_237 :: Chunk
c256_f_237 = fc256 c256_237

c256_f_238 :: Chunk
c256_f_238 = fc256 c256_238

c256_f_239 :: Chunk
c256_f_239 = fc256 c256_239

c256_f_240 :: Chunk
c256_f_240 = fc256 c256_240

c256_f_241 :: Chunk
c256_f_241 = fc256 c256_241

c256_f_242 :: Chunk
c256_f_242 = fc256 c256_242

c256_f_243 :: Chunk
c256_f_243 = fc256 c256_243

c256_f_244 :: Chunk
c256_f_244 = fc256 c256_244

c256_f_245 :: Chunk
c256_f_245 = fc256 c256_245

c256_f_246 :: Chunk
c256_f_246 = fc256 c256_246

c256_f_247 :: Chunk
c256_f_247 = fc256 c256_247

c256_f_248 :: Chunk
c256_f_248 = fc256 c256_248

c256_f_249 :: Chunk
c256_f_249 = fc256 c256_249

c256_f_250 :: Chunk
c256_f_250 = fc256 c256_250

c256_f_251 :: Chunk
c256_f_251 = fc256 c256_251

c256_f_252 :: Chunk
c256_f_252 = fc256 c256_252

c256_f_253 :: Chunk
c256_f_253 = fc256 c256_253

c256_f_254 :: Chunk
c256_f_254 = fc256 c256_254

c256_f_255 :: Chunk
c256_f_255 = fc256 c256_255

-- * 256 colors, background, numbered

c256_b_0 :: Chunk
c256_b_0 = bc256 c256_0

c256_b_1 :: Chunk
c256_b_1 = bc256 c256_1

c256_b_2 :: Chunk
c256_b_2 = bc256 c256_2

c256_b_3 :: Chunk
c256_b_3 = bc256 c256_3

c256_b_4 :: Chunk
c256_b_4 = bc256 c256_4

c256_b_5 :: Chunk
c256_b_5 = bc256 c256_5

c256_b_6 :: Chunk
c256_b_6 = bc256 c256_6

c256_b_7 :: Chunk
c256_b_7 = bc256 c256_7

c256_b_8 :: Chunk
c256_b_8 = bc256 c256_8

c256_b_9 :: Chunk
c256_b_9 = bc256 c256_9

c256_b_10 :: Chunk
c256_b_10 = bc256 c256_10

c256_b_11 :: Chunk
c256_b_11 = bc256 c256_11

c256_b_12 :: Chunk
c256_b_12 = bc256 c256_12

c256_b_13 :: Chunk
c256_b_13 = bc256 c256_13

c256_b_14 :: Chunk
c256_b_14 = bc256 c256_14

c256_b_15 :: Chunk
c256_b_15 = bc256 c256_15

c256_b_16 :: Chunk
c256_b_16 = bc256 c256_16

c256_b_17 :: Chunk
c256_b_17 = bc256 c256_17

c256_b_18 :: Chunk
c256_b_18 = bc256 c256_18

c256_b_19 :: Chunk
c256_b_19 = bc256 c256_19

c256_b_20 :: Chunk
c256_b_20 = bc256 c256_20

c256_b_21 :: Chunk
c256_b_21 = bc256 c256_21

c256_b_22 :: Chunk
c256_b_22 = bc256 c256_22

c256_b_23 :: Chunk
c256_b_23 = bc256 c256_23

c256_b_24 :: Chunk
c256_b_24 = bc256 c256_24

c256_b_25 :: Chunk
c256_b_25 = bc256 c256_25

c256_b_26 :: Chunk
c256_b_26 = bc256 c256_26

c256_b_27 :: Chunk
c256_b_27 = bc256 c256_27

c256_b_28 :: Chunk
c256_b_28 = bc256 c256_28

c256_b_29 :: Chunk
c256_b_29 = bc256 c256_29

c256_b_30 :: Chunk
c256_b_30 = bc256 c256_30

c256_b_31 :: Chunk
c256_b_31 = bc256 c256_31

c256_b_32 :: Chunk
c256_b_32 = bc256 c256_32

c256_b_33 :: Chunk
c256_b_33 = bc256 c256_33

c256_b_34 :: Chunk
c256_b_34 = bc256 c256_34

c256_b_35 :: Chunk
c256_b_35 = bc256 c256_35

c256_b_36 :: Chunk
c256_b_36 = bc256 c256_36

c256_b_37 :: Chunk
c256_b_37 = bc256 c256_37

c256_b_38 :: Chunk
c256_b_38 = bc256 c256_38

c256_b_39 :: Chunk
c256_b_39 = bc256 c256_39

c256_b_40 :: Chunk
c256_b_40 = bc256 c256_40

c256_b_41 :: Chunk
c256_b_41 = bc256 c256_41

c256_b_42 :: Chunk
c256_b_42 = bc256 c256_42

c256_b_43 :: Chunk
c256_b_43 = bc256 c256_43

c256_b_44 :: Chunk
c256_b_44 = bc256 c256_44

c256_b_45 :: Chunk
c256_b_45 = bc256 c256_45

c256_b_46 :: Chunk
c256_b_46 = bc256 c256_46

c256_b_47 :: Chunk
c256_b_47 = bc256 c256_47

c256_b_48 :: Chunk
c256_b_48 = bc256 c256_48

c256_b_49 :: Chunk
c256_b_49 = bc256 c256_49

c256_b_50 :: Chunk
c256_b_50 = bc256 c256_50

c256_b_51 :: Chunk
c256_b_51 = bc256 c256_51

c256_b_52 :: Chunk
c256_b_52 = bc256 c256_52

c256_b_53 :: Chunk
c256_b_53 = bc256 c256_53

c256_b_54 :: Chunk
c256_b_54 = bc256 c256_54

c256_b_55 :: Chunk
c256_b_55 = bc256 c256_55

c256_b_56 :: Chunk
c256_b_56 = bc256 c256_56

c256_b_57 :: Chunk
c256_b_57 = bc256 c256_57

c256_b_58 :: Chunk
c256_b_58 = bc256 c256_58

c256_b_59 :: Chunk
c256_b_59 = bc256 c256_59

c256_b_60 :: Chunk
c256_b_60 = bc256 c256_60

c256_b_61 :: Chunk
c256_b_61 = bc256 c256_61

c256_b_62 :: Chunk
c256_b_62 = bc256 c256_62

c256_b_63 :: Chunk
c256_b_63 = bc256 c256_63

c256_b_64 :: Chunk
c256_b_64 = bc256 c256_64

c256_b_65 :: Chunk
c256_b_65 = bc256 c256_65

c256_b_66 :: Chunk
c256_b_66 = bc256 c256_66

c256_b_67 :: Chunk
c256_b_67 = bc256 c256_67

c256_b_68 :: Chunk
c256_b_68 = bc256 c256_68

c256_b_69 :: Chunk
c256_b_69 = bc256 c256_69

c256_b_70 :: Chunk
c256_b_70 = bc256 c256_70

c256_b_71 :: Chunk
c256_b_71 = bc256 c256_71

c256_b_72 :: Chunk
c256_b_72 = bc256 c256_72

c256_b_73 :: Chunk
c256_b_73 = bc256 c256_73

c256_b_74 :: Chunk
c256_b_74 = bc256 c256_74

c256_b_75 :: Chunk
c256_b_75 = bc256 c256_75

c256_b_76 :: Chunk
c256_b_76 = bc256 c256_76

c256_b_77 :: Chunk
c256_b_77 = bc256 c256_77

c256_b_78 :: Chunk
c256_b_78 = bc256 c256_78

c256_b_79 :: Chunk
c256_b_79 = bc256 c256_79

c256_b_80 :: Chunk
c256_b_80 = bc256 c256_80

c256_b_81 :: Chunk
c256_b_81 = bc256 c256_81

c256_b_82 :: Chunk
c256_b_82 = bc256 c256_82

c256_b_83 :: Chunk
c256_b_83 = bc256 c256_83

c256_b_84 :: Chunk
c256_b_84 = bc256 c256_84

c256_b_85 :: Chunk
c256_b_85 = bc256 c256_85

c256_b_86 :: Chunk
c256_b_86 = bc256 c256_86

c256_b_87 :: Chunk
c256_b_87 = bc256 c256_87

c256_b_88 :: Chunk
c256_b_88 = bc256 c256_88

c256_b_89 :: Chunk
c256_b_89 = bc256 c256_89

c256_b_90 :: Chunk
c256_b_90 = bc256 c256_90

c256_b_91 :: Chunk
c256_b_91 = bc256 c256_91

c256_b_92 :: Chunk
c256_b_92 = bc256 c256_92

c256_b_93 :: Chunk
c256_b_93 = bc256 c256_93

c256_b_94 :: Chunk
c256_b_94 = bc256 c256_94

c256_b_95 :: Chunk
c256_b_95 = bc256 c256_95

c256_b_96 :: Chunk
c256_b_96 = bc256 c256_96

c256_b_97 :: Chunk
c256_b_97 = bc256 c256_97

c256_b_98 :: Chunk
c256_b_98 = bc256 c256_98

c256_b_99 :: Chunk
c256_b_99 = bc256 c256_99

c256_b_100 :: Chunk
c256_b_100 = bc256 c256_100

c256_b_101 :: Chunk
c256_b_101 = bc256 c256_101

c256_b_102 :: Chunk
c256_b_102 = bc256 c256_102

c256_b_103 :: Chunk
c256_b_103 = bc256 c256_103

c256_b_104 :: Chunk
c256_b_104 = bc256 c256_104

c256_b_105 :: Chunk
c256_b_105 = bc256 c256_105

c256_b_106 :: Chunk
c256_b_106 = bc256 c256_106

c256_b_107 :: Chunk
c256_b_107 = bc256 c256_107

c256_b_108 :: Chunk
c256_b_108 = bc256 c256_108

c256_b_109 :: Chunk
c256_b_109 = bc256 c256_109

c256_b_110 :: Chunk
c256_b_110 = bc256 c256_110

c256_b_111 :: Chunk
c256_b_111 = bc256 c256_111

c256_b_112 :: Chunk
c256_b_112 = bc256 c256_112

c256_b_113 :: Chunk
c256_b_113 = bc256 c256_113

c256_b_114 :: Chunk
c256_b_114 = bc256 c256_114

c256_b_115 :: Chunk
c256_b_115 = bc256 c256_115

c256_b_116 :: Chunk
c256_b_116 = bc256 c256_116

c256_b_117 :: Chunk
c256_b_117 = bc256 c256_117

c256_b_118 :: Chunk
c256_b_118 = bc256 c256_118

c256_b_119 :: Chunk
c256_b_119 = bc256 c256_119

c256_b_120 :: Chunk
c256_b_120 = bc256 c256_120

c256_b_121 :: Chunk
c256_b_121 = bc256 c256_121

c256_b_122 :: Chunk
c256_b_122 = bc256 c256_122

c256_b_123 :: Chunk
c256_b_123 = bc256 c256_123

c256_b_124 :: Chunk
c256_b_124 = bc256 c256_124

c256_b_125 :: Chunk
c256_b_125 = bc256 c256_125

c256_b_126 :: Chunk
c256_b_126 = bc256 c256_126

c256_b_127 :: Chunk
c256_b_127 = bc256 c256_127

c256_b_128 :: Chunk
c256_b_128 = bc256 c256_128

c256_b_129 :: Chunk
c256_b_129 = bc256 c256_129

c256_b_130 :: Chunk
c256_b_130 = bc256 c256_130

c256_b_131 :: Chunk
c256_b_131 = bc256 c256_131

c256_b_132 :: Chunk
c256_b_132 = bc256 c256_132

c256_b_133 :: Chunk
c256_b_133 = bc256 c256_133

c256_b_134 :: Chunk
c256_b_134 = bc256 c256_134

c256_b_135 :: Chunk
c256_b_135 = bc256 c256_135

c256_b_136 :: Chunk
c256_b_136 = bc256 c256_136

c256_b_137 :: Chunk
c256_b_137 = bc256 c256_137

c256_b_138 :: Chunk
c256_b_138 = bc256 c256_138

c256_b_139 :: Chunk
c256_b_139 = bc256 c256_139

c256_b_140 :: Chunk
c256_b_140 = bc256 c256_140

c256_b_141 :: Chunk
c256_b_141 = bc256 c256_141

c256_b_142 :: Chunk
c256_b_142 = bc256 c256_142

c256_b_143 :: Chunk
c256_b_143 = bc256 c256_143

c256_b_144 :: Chunk
c256_b_144 = bc256 c256_144

c256_b_145 :: Chunk
c256_b_145 = bc256 c256_145

c256_b_146 :: Chunk
c256_b_146 = bc256 c256_146

c256_b_147 :: Chunk
c256_b_147 = bc256 c256_147

c256_b_148 :: Chunk
c256_b_148 = bc256 c256_148

c256_b_149 :: Chunk
c256_b_149 = bc256 c256_149

c256_b_150 :: Chunk
c256_b_150 = bc256 c256_150

c256_b_151 :: Chunk
c256_b_151 = bc256 c256_151

c256_b_152 :: Chunk
c256_b_152 = bc256 c256_152

c256_b_153 :: Chunk
c256_b_153 = bc256 c256_153

c256_b_154 :: Chunk
c256_b_154 = bc256 c256_154

c256_b_155 :: Chunk
c256_b_155 = bc256 c256_155

c256_b_156 :: Chunk
c256_b_156 = bc256 c256_156

c256_b_157 :: Chunk
c256_b_157 = bc256 c256_157

c256_b_158 :: Chunk
c256_b_158 = bc256 c256_158

c256_b_159 :: Chunk
c256_b_159 = bc256 c256_159

c256_b_160 :: Chunk
c256_b_160 = bc256 c256_160

c256_b_161 :: Chunk
c256_b_161 = bc256 c256_161

c256_b_162 :: Chunk
c256_b_162 = bc256 c256_162

c256_b_163 :: Chunk
c256_b_163 = bc256 c256_163

c256_b_164 :: Chunk
c256_b_164 = bc256 c256_164

c256_b_165 :: Chunk
c256_b_165 = bc256 c256_165

c256_b_166 :: Chunk
c256_b_166 = bc256 c256_166

c256_b_167 :: Chunk
c256_b_167 = bc256 c256_167

c256_b_168 :: Chunk
c256_b_168 = bc256 c256_168

c256_b_169 :: Chunk
c256_b_169 = bc256 c256_169

c256_b_170 :: Chunk
c256_b_170 = bc256 c256_170

c256_b_171 :: Chunk
c256_b_171 = bc256 c256_171

c256_b_172 :: Chunk
c256_b_172 = bc256 c256_172

c256_b_173 :: Chunk
c256_b_173 = bc256 c256_173

c256_b_174 :: Chunk
c256_b_174 = bc256 c256_174

c256_b_175 :: Chunk
c256_b_175 = bc256 c256_175

c256_b_176 :: Chunk
c256_b_176 = bc256 c256_176

c256_b_177 :: Chunk
c256_b_177 = bc256 c256_177

c256_b_178 :: Chunk
c256_b_178 = bc256 c256_178

c256_b_179 :: Chunk
c256_b_179 = bc256 c256_179

c256_b_180 :: Chunk
c256_b_180 = bc256 c256_180

c256_b_181 :: Chunk
c256_b_181 = bc256 c256_181

c256_b_182 :: Chunk
c256_b_182 = bc256 c256_182

c256_b_183 :: Chunk
c256_b_183 = bc256 c256_183

c256_b_184 :: Chunk
c256_b_184 = bc256 c256_184

c256_b_185 :: Chunk
c256_b_185 = bc256 c256_185

c256_b_186 :: Chunk
c256_b_186 = bc256 c256_186

c256_b_187 :: Chunk
c256_b_187 = bc256 c256_187

c256_b_188 :: Chunk
c256_b_188 = bc256 c256_188

c256_b_189 :: Chunk
c256_b_189 = bc256 c256_189

c256_b_190 :: Chunk
c256_b_190 = bc256 c256_190

c256_b_191 :: Chunk
c256_b_191 = bc256 c256_191

c256_b_192 :: Chunk
c256_b_192 = bc256 c256_192

c256_b_193 :: Chunk
c256_b_193 = bc256 c256_193

c256_b_194 :: Chunk
c256_b_194 = bc256 c256_194

c256_b_195 :: Chunk
c256_b_195 = bc256 c256_195

c256_b_196 :: Chunk
c256_b_196 = bc256 c256_196

c256_b_197 :: Chunk
c256_b_197 = bc256 c256_197

c256_b_198 :: Chunk
c256_b_198 = bc256 c256_198

c256_b_199 :: Chunk
c256_b_199 = bc256 c256_199

c256_b_200 :: Chunk
c256_b_200 = bc256 c256_200

c256_b_201 :: Chunk
c256_b_201 = bc256 c256_201

c256_b_202 :: Chunk
c256_b_202 = bc256 c256_202

c256_b_203 :: Chunk
c256_b_203 = bc256 c256_203

c256_b_204 :: Chunk
c256_b_204 = bc256 c256_204

c256_b_205 :: Chunk
c256_b_205 = bc256 c256_205

c256_b_206 :: Chunk
c256_b_206 = bc256 c256_206

c256_b_207 :: Chunk
c256_b_207 = bc256 c256_207

c256_b_208 :: Chunk
c256_b_208 = bc256 c256_208

c256_b_209 :: Chunk
c256_b_209 = bc256 c256_209

c256_b_210 :: Chunk
c256_b_210 = bc256 c256_210

c256_b_211 :: Chunk
c256_b_211 = bc256 c256_211

c256_b_212 :: Chunk
c256_b_212 = bc256 c256_212

c256_b_213 :: Chunk
c256_b_213 = bc256 c256_213

c256_b_214 :: Chunk
c256_b_214 = bc256 c256_214

c256_b_215 :: Chunk
c256_b_215 = bc256 c256_215

c256_b_216 :: Chunk
c256_b_216 = bc256 c256_216

c256_b_217 :: Chunk
c256_b_217 = bc256 c256_217

c256_b_218 :: Chunk
c256_b_218 = bc256 c256_218

c256_b_219 :: Chunk
c256_b_219 = bc256 c256_219

c256_b_220 :: Chunk
c256_b_220 = bc256 c256_220

c256_b_221 :: Chunk
c256_b_221 = bc256 c256_221

c256_b_222 :: Chunk
c256_b_222 = bc256 c256_222

c256_b_223 :: Chunk
c256_b_223 = bc256 c256_223

c256_b_224 :: Chunk
c256_b_224 = bc256 c256_224

c256_b_225 :: Chunk
c256_b_225 = bc256 c256_225

c256_b_226 :: Chunk
c256_b_226 = bc256 c256_226

c256_b_227 :: Chunk
c256_b_227 = bc256 c256_227

c256_b_228 :: Chunk
c256_b_228 = bc256 c256_228

c256_b_229 :: Chunk
c256_b_229 = bc256 c256_229

c256_b_230 :: Chunk
c256_b_230 = bc256 c256_230

c256_b_231 :: Chunk
c256_b_231 = bc256 c256_231

c256_b_232 :: Chunk
c256_b_232 = bc256 c256_232

c256_b_233 :: Chunk
c256_b_233 = bc256 c256_233

c256_b_234 :: Chunk
c256_b_234 = bc256 c256_234

c256_b_235 :: Chunk
c256_b_235 = bc256 c256_235

c256_b_236 :: Chunk
c256_b_236 = bc256 c256_236

c256_b_237 :: Chunk
c256_b_237 = bc256 c256_237

c256_b_238 :: Chunk
c256_b_238 = bc256 c256_238

c256_b_239 :: Chunk
c256_b_239 = bc256 c256_239

c256_b_240 :: Chunk
c256_b_240 = bc256 c256_240

c256_b_241 :: Chunk
c256_b_241 = bc256 c256_241

c256_b_242 :: Chunk
c256_b_242 = bc256 c256_242

c256_b_243 :: Chunk
c256_b_243 = bc256 c256_243

c256_b_244 :: Chunk
c256_b_244 = bc256 c256_244

c256_b_245 :: Chunk
c256_b_245 = bc256 c256_245

c256_b_246 :: Chunk
c256_b_246 = bc256 c256_246

c256_b_247 :: Chunk
c256_b_247 = bc256 c256_247

c256_b_248 :: Chunk
c256_b_248 = bc256 c256_248

c256_b_249 :: Chunk
c256_b_249 = bc256 c256_249

c256_b_250 :: Chunk
c256_b_250 = bc256 c256_250

c256_b_251 :: Chunk
c256_b_251 = bc256 c256_251

c256_b_252 :: Chunk
c256_b_252 = bc256 c256_252

c256_b_253 :: Chunk
c256_b_253 = bc256 c256_253

c256_b_254 :: Chunk
c256_b_254 = bc256 c256_254

c256_b_255 :: Chunk
c256_b_255 = bc256 c256_255


-- * Utility functions

-- | Make a Color into a 8-color foreground Chunk.
fc8 :: Color8 -> Chunk
fc8 c = x { textSpec = (textSpec x) {
  style8 = (style8 (textSpec x)) {
    foreground8 = Last . Just $ c }}}
  where
    x = mempty

-- | Make a terminfo Color into a 8-color background Chunk.
bc8 :: Color8 -> Chunk
bc8 c = x { textSpec = (textSpec x) {
  style8 = (style8 (textSpec x)) {
    background8 = Last . Just $ c }}}
  where
    x = mempty

-- | Make a terminfo Color into a 256-color foreground Chunk.
fc256 :: Color256 -> Chunk
fc256 c = x { textSpec = (textSpec x) {
  style256 = (style256 (textSpec x)) {
    foreground256 = Last . Just $ c }}}
  where
    x = mempty

-- | Make a terminfo Color into a 256-color background Chunk.
bc256 :: Color256 -> Chunk
bc256 c = x { textSpec = (textSpec x) {
  style256 = (style256 (textSpec x)) {
    background256 = Last . Just $ c }}}
  where
    x = mempty
