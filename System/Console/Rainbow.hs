{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
-- | Handles colors and special effects for text. Internally this
-- module uses the Haskell terminfo library, which links against the
-- UNIX library of the same name, so it should work with a wide
-- variety of UNIX terminals.
--
-- The building block of Rainbow is the 'Chunk'. Each Chunk comes with
-- a 'TextSpec', which specifies how the text should look on 8-color
-- and on 256-color terminals. The Chunk is a full specification; that
-- is, although Chunks are typically printed one after the other, the
-- appearance of one Chunk does not affect the appearance of the next
-- Chunk.
--
-- You have full freedom to specify different attributes and colors
-- for 8 and 256 color terminals; for instance, you can have text
-- appear red on an 8-color terminal but blue on a 256-color terminal.
--
-- Some useful combinators are provided to assist with the building of
-- Chunks. 'plain' builds a Chunk with a provided 'Text' that is
-- rendered using the terminal's default settings and colors. You then
-- use '+.+' to combine different modifiers to change how the Chunk is
-- rendered. Here's an example:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > -- This chunk is blue and underlined, both on 8-color and 256-color
-- > -- terminals.
-- > blueHello :: Chunk
-- > blueHello = plain "Hello world!" +.+ f_blue +.+ underline
-- >
-- > -- This chunk is red on 8-color terminals but uses color 88 on
-- > -- 256-color terminals. Because +.+ is left-associative, the
-- > -- color256_f_88 supersedes the f_red, which sets the foreground
-- > -- on both 8 and 256 color terminals to red.
-- > redHello :: Chunk
-- > redHello = plain "Hello world!" +.+ f_red +.+ color256_f_88
-- >
-- > -- This chunk is underlined on 8-color terminals but is not
-- > -- underlined on 256-color terminals.
-- > underlinedOn8 :: Chunk
-- > underlinedOn8 = plain "Hello world!" +.+ underline8
-- >
-- > newline :: Chunk
-- > newline = plain "\n"
-- >
-- > -- How to print all these chunks
-- > main :: IO ()
-- > main = do
-- >   t <- termFromEnv
-- >   printChunks t [blueHello, nl, redHello, nl, underlinedOn8, nl]


module System.Console.Rainbow (

  -- * Terminal definitions
    Term(..)
  , termFromEnv
  , smartTermFromEnv

  -- * Chunks
  , Chunk
  , M.Monoid(..)
  , (<>)

  -- * Printing chunks
  , printChunks
  , hPrintChunks

  -- * Effects for both 8 and 256 color terminals

  -- | These modifiers affect both 8 and 256 color terminals:
  --
  -- > {-# LANGUAGE OverloadedStrings #-}
  -- > underlinedOn8and256 :: Chunk
  -- > underlinedOn8and256 = plain "Underlined!" +.+ underline
  --
  -- There are also modifiers to turn an effect off, such as
  -- 'boldOff'. Ordinarily you will not need these because each chunk
  -- starts with no effects, so you only need to turn on the effects
  -- you want. However the @off@ modifiers are here if you need them.

  , bold, boldOff
  , underline, underlineOff
  , flash, flashOff
  , inverse, inverseOff

  -- * Effects for 8-color terminals only

  -- | These modifiers affect 8-color terminals only. For instance
  -- this appears bold only on an 8-color terminal:
  --
  -- > {-# LANGUAGE OverloadedStrings #-}
  -- > boldOn8 :: Chunk
  -- > boldOn8 = plain "Bold on 8 color terminal only" +.+ bold8

  , bold8, bold8off
  , underline8, underline8off
  , flash8, flash8off
  , inverse8, inverse8off

  -- * Effects for 256-color terminals only

  -- | These modifiers affect 256-color terminals only. For instance,
  -- this text is underlined on 256 color terminals but is bold on
  -- 8-color terminals:
  --
  -- > {-# LANGUAGE OverloadedStrings #-}
  -- > underlinedOn256 :: Chunk
  -- > underlinedOn256 = plain "Underlined on 256 color terminal"
  -- >                 +.+ underlined256 +.+ bold8

  , bold256, bold256off
  , underline256, underline256off
  , flash256, flash256off
  , inverse256, inverse256off

  -- * Colors for both 8 and 256 color terminals

  -- | These color modifiers affect both 8 and 256 color
  -- terminals. For example, to print something in red on blue on both
  -- an 8 and a 256 color terminal:
  --
  -- > {-# LANGUAGE OverloadedStrings #-}
  -- > redHello :: Chunk
  -- > redHello = plain "Hello world!" +.+ f_red +.+ b_blue

  -- ** Foreground colors

  , f_default
  , f_black
  , f_red
  , f_green
  , f_yellow
  , f_blue
  , f_magenta
  , f_cyan
  , f_white

  -- ** Background colors

  , b_default
  , b_black
  , b_red
  , b_green
  , b_yellow
  , b_blue
  , b_magenta
  , b_cyan
  , b_white

   -- * Specific colors
   -- ** 8 color foreground colors
  , color8_f_default
  , color8_f_black
  , color8_f_red
  , color8_f_green
  , color8_f_yellow
  , color8_f_blue
  , color8_f_magenta
  , color8_f_cyan
  , color8_f_white

   -- ** 8 color background colors
  , color8_b_default
  , color8_b_black
  , color8_b_red
  , color8_b_green
  , color8_b_yellow
  , color8_b_blue
  , color8_b_magenta
  , color8_b_cyan
  , color8_b_white

   -- ** 256 color foreground colors

   -- | The color names assume a palette similar to the default one
   -- that xterm uses.
  , color256_f_default
  , color256_f_0
  , color256_f_black
  , color256_f_1
  , color256_f_red
  , color256_f_2
  , color256_f_green
  , color256_f_3
  , color256_f_yellow
  , color256_f_4
  , color256_f_blue
  , color256_f_5
  , color256_f_magenta
  , color256_f_6
  , color256_f_cyan
  , color256_f_7
  , color256_f_white
  , color256_f_8
  , color256_f_grey
  , color256_f_9
  , color256_f_red_bright
  , color256_f_10
  , color256_f_green_bright
  , color256_f_11
  , color256_f_yellow_bright
  , color256_f_12
  , color256_f_blue_bright
  , color256_f_13
  , color256_f_magenta_bright
  , color256_f_14
  , color256_f_cyan_bright
  , color256_f_15
  , color256_f_white_bright
  , color256_f_16
  , color256_f_17
  , color256_f_18
  , color256_f_19
  , color256_f_20
  , color256_f_21
  , color256_f_22
  , color256_f_23
  , color256_f_24
  , color256_f_25
  , color256_f_26
  , color256_f_27
  , color256_f_28
  , color256_f_29
  , color256_f_30
  , color256_f_31
  , color256_f_32
  , color256_f_33
  , color256_f_34
  , color256_f_35
  , color256_f_36
  , color256_f_37
  , color256_f_38
  , color256_f_39
  , color256_f_40
  , color256_f_41
  , color256_f_42
  , color256_f_43
  , color256_f_44
  , color256_f_45
  , color256_f_46
  , color256_f_47
  , color256_f_48
  , color256_f_49
  , color256_f_50
  , color256_f_51
  , color256_f_52
  , color256_f_53
  , color256_f_54
  , color256_f_55
  , color256_f_56
  , color256_f_57
  , color256_f_58
  , color256_f_59
  , color256_f_60
  , color256_f_61
  , color256_f_62
  , color256_f_63
  , color256_f_64
  , color256_f_65
  , color256_f_66
  , color256_f_67
  , color256_f_68
  , color256_f_69
  , color256_f_70
  , color256_f_71
  , color256_f_72
  , color256_f_73
  , color256_f_74
  , color256_f_75
  , color256_f_76
  , color256_f_77
  , color256_f_78
  , color256_f_79
  , color256_f_80
  , color256_f_81
  , color256_f_82
  , color256_f_83
  , color256_f_84
  , color256_f_85
  , color256_f_86
  , color256_f_87
  , color256_f_88
  , color256_f_89
  , color256_f_90
  , color256_f_91
  , color256_f_92
  , color256_f_93
  , color256_f_94
  , color256_f_95
  , color256_f_96
  , color256_f_97
  , color256_f_98
  , color256_f_99
  , color256_f_100
  , color256_f_101
  , color256_f_102
  , color256_f_103
  , color256_f_104
  , color256_f_105
  , color256_f_106
  , color256_f_107
  , color256_f_108
  , color256_f_109
  , color256_f_110
  , color256_f_111
  , color256_f_112
  , color256_f_113
  , color256_f_114
  , color256_f_115
  , color256_f_116
  , color256_f_117
  , color256_f_118
  , color256_f_119
  , color256_f_120
  , color256_f_121
  , color256_f_122
  , color256_f_123
  , color256_f_124
  , color256_f_125
  , color256_f_126
  , color256_f_127
  , color256_f_128
  , color256_f_129
  , color256_f_130
  , color256_f_131
  , color256_f_132
  , color256_f_133
  , color256_f_134
  , color256_f_135
  , color256_f_136
  , color256_f_137
  , color256_f_138
  , color256_f_139
  , color256_f_140
  , color256_f_141
  , color256_f_142
  , color256_f_143
  , color256_f_144
  , color256_f_145
  , color256_f_146
  , color256_f_147
  , color256_f_148
  , color256_f_149
  , color256_f_150
  , color256_f_151
  , color256_f_152
  , color256_f_153
  , color256_f_154
  , color256_f_155
  , color256_f_156
  , color256_f_157
  , color256_f_158
  , color256_f_159
  , color256_f_160
  , color256_f_161
  , color256_f_162
  , color256_f_163
  , color256_f_164
  , color256_f_165
  , color256_f_166
  , color256_f_167
  , color256_f_168
  , color256_f_169
  , color256_f_170
  , color256_f_171
  , color256_f_172
  , color256_f_173
  , color256_f_174
  , color256_f_175
  , color256_f_176
  , color256_f_177
  , color256_f_178
  , color256_f_179
  , color256_f_180
  , color256_f_181
  , color256_f_182
  , color256_f_183
  , color256_f_184
  , color256_f_185
  , color256_f_186
  , color256_f_187
  , color256_f_188
  , color256_f_189
  , color256_f_190
  , color256_f_191
  , color256_f_192
  , color256_f_193
  , color256_f_194
  , color256_f_195
  , color256_f_196
  , color256_f_197
  , color256_f_198
  , color256_f_199
  , color256_f_200
  , color256_f_201
  , color256_f_202
  , color256_f_203
  , color256_f_204
  , color256_f_205
  , color256_f_206
  , color256_f_207
  , color256_f_208
  , color256_f_209
  , color256_f_210
  , color256_f_211
  , color256_f_212
  , color256_f_213
  , color256_f_214
  , color256_f_215
  , color256_f_216
  , color256_f_217
  , color256_f_218
  , color256_f_219
  , color256_f_220
  , color256_f_221
  , color256_f_222
  , color256_f_223
  , color256_f_224
  , color256_f_225
  , color256_f_226
  , color256_f_227
  , color256_f_228
  , color256_f_229
  , color256_f_230
  , color256_f_231
  , color256_f_232
  , color256_f_233
  , color256_f_234
  , color256_f_235
  , color256_f_236
  , color256_f_237
  , color256_f_238
  , color256_f_239
  , color256_f_240
  , color256_f_241
  , color256_f_242
  , color256_f_243
  , color256_f_244
  , color256_f_245
  , color256_f_246
  , color256_f_247
  , color256_f_248
  , color256_f_249
  , color256_f_250
  , color256_f_251
  , color256_f_252
  , color256_f_253
  , color256_f_254
  , color256_f_255

  -- ** 256 color background colors

   -- | The color names assume a palette similar to the default one
   -- that xterm uses.
  , color256_b_default
  , color256_b_0
  , color256_b_black
  , color256_b_1
  , color256_b_red
  , color256_b_2
  , color256_b_green
  , color256_b_3
  , color256_b_yellow
  , color256_b_4
  , color256_b_blue
  , color256_b_5
  , color256_b_magenta
  , color256_b_6
  , color256_b_cyan
  , color256_b_7
  , color256_b_white
  , color256_b_8
  , color256_b_grey
  , color256_b_9
  , color256_b_red_bright
  , color256_b_10
  , color256_b_green_bright
  , color256_b_11
  , color256_b_yellow_bright
  , color256_b_12
  , color256_b_blue_bright
  , color256_b_13
  , color256_b_magenta_bright
  , color256_b_14
  , color256_b_cyan_bright
  , color256_b_15
  , color256_b_white_bright
  , color256_b_16
  , color256_b_17
  , color256_b_18
  , color256_b_19
  , color256_b_20
  , color256_b_21
  , color256_b_22
  , color256_b_23
  , color256_b_24
  , color256_b_25
  , color256_b_26
  , color256_b_27
  , color256_b_28
  , color256_b_29
  , color256_b_30
  , color256_b_31
  , color256_b_32
  , color256_b_33
  , color256_b_34
  , color256_b_35
  , color256_b_36
  , color256_b_37
  , color256_b_38
  , color256_b_39
  , color256_b_40
  , color256_b_41
  , color256_b_42
  , color256_b_43
  , color256_b_44
  , color256_b_45
  , color256_b_46
  , color256_b_47
  , color256_b_48
  , color256_b_49
  , color256_b_50
  , color256_b_51
  , color256_b_52
  , color256_b_53
  , color256_b_54
  , color256_b_55
  , color256_b_56
  , color256_b_57
  , color256_b_58
  , color256_b_59
  , color256_b_60
  , color256_b_61
  , color256_b_62
  , color256_b_63
  , color256_b_64
  , color256_b_65
  , color256_b_66
  , color256_b_67
  , color256_b_68
  , color256_b_69
  , color256_b_70
  , color256_b_71
  , color256_b_72
  , color256_b_73
  , color256_b_74
  , color256_b_75
  , color256_b_76
  , color256_b_77
  , color256_b_78
  , color256_b_79
  , color256_b_80
  , color256_b_81
  , color256_b_82
  , color256_b_83
  , color256_b_84
  , color256_b_85
  , color256_b_86
  , color256_b_87
  , color256_b_88
  , color256_b_89
  , color256_b_90
  , color256_b_91
  , color256_b_92
  , color256_b_93
  , color256_b_94
  , color256_b_95
  , color256_b_96
  , color256_b_97
  , color256_b_98
  , color256_b_99
  , color256_b_100
  , color256_b_101
  , color256_b_102
  , color256_b_103
  , color256_b_104
  , color256_b_105
  , color256_b_106
  , color256_b_107
  , color256_b_108
  , color256_b_109
  , color256_b_110
  , color256_b_111
  , color256_b_112
  , color256_b_113
  , color256_b_114
  , color256_b_115
  , color256_b_116
  , color256_b_117
  , color256_b_118
  , color256_b_119
  , color256_b_120
  , color256_b_121
  , color256_b_122
  , color256_b_123
  , color256_b_124
  , color256_b_125
  , color256_b_126
  , color256_b_127
  , color256_b_128
  , color256_b_129
  , color256_b_130
  , color256_b_131
  , color256_b_132
  , color256_b_133
  , color256_b_134
  , color256_b_135
  , color256_b_136
  , color256_b_137
  , color256_b_138
  , color256_b_139
  , color256_b_140
  , color256_b_141
  , color256_b_142
  , color256_b_143
  , color256_b_144
  , color256_b_145
  , color256_b_146
  , color256_b_147
  , color256_b_148
  , color256_b_149
  , color256_b_150
  , color256_b_151
  , color256_b_152
  , color256_b_153
  , color256_b_154
  , color256_b_155
  , color256_b_156
  , color256_b_157
  , color256_b_158
  , color256_b_159
  , color256_b_160
  , color256_b_161
  , color256_b_162
  , color256_b_163
  , color256_b_164
  , color256_b_165
  , color256_b_166
  , color256_b_167
  , color256_b_168
  , color256_b_169
  , color256_b_170
  , color256_b_171
  , color256_b_172
  , color256_b_173
  , color256_b_174
  , color256_b_175
  , color256_b_176
  , color256_b_177
  , color256_b_178
  , color256_b_179
  , color256_b_180
  , color256_b_181
  , color256_b_182
  , color256_b_183
  , color256_b_184
  , color256_b_185
  , color256_b_186
  , color256_b_187
  , color256_b_188
  , color256_b_189
  , color256_b_190
  , color256_b_191
  , color256_b_192
  , color256_b_193
  , color256_b_194
  , color256_b_195
  , color256_b_196
  , color256_b_197
  , color256_b_198
  , color256_b_199
  , color256_b_200
  , color256_b_201
  , color256_b_202
  , color256_b_203
  , color256_b_204
  , color256_b_205
  , color256_b_206
  , color256_b_207
  , color256_b_208
  , color256_b_209
  , color256_b_210
  , color256_b_211
  , color256_b_212
  , color256_b_213
  , color256_b_214
  , color256_b_215
  , color256_b_216
  , color256_b_217
  , color256_b_218
  , color256_b_219
  , color256_b_220
  , color256_b_221
  , color256_b_222
  , color256_b_223
  , color256_b_224
  , color256_b_225
  , color256_b_226
  , color256_b_227
  , color256_b_228
  , color256_b_229
  , color256_b_230
  , color256_b_231
  , color256_b_232
  , color256_b_233
  , color256_b_234
  , color256_b_235
  , color256_b_236
  , color256_b_237
  , color256_b_238
  , color256_b_239
  , color256_b_240
  , color256_b_241
  , color256_b_242
  , color256_b_243
  , color256_b_244
  , color256_b_245
  , color256_b_246
  , color256_b_247
  , color256_b_248
  , color256_b_249
  , color256_b_250
  , color256_b_251
  , color256_b_252
  , color256_b_253
  , color256_b_254
  , color256_b_255

  -- * Create your own colors

  -- * Style, TextSpec, and Chunk innards

  -- | A style is a bundle of attributes that describes text
  -- attributes, such as its color and whether it is bold.
  --
  -- Ordinarily you shouldn't need to use these types but they are
  -- here in case they are useful; in particular, you can use them to
  -- examine a Chunk's TextSpec to see what its characteristics are.
  , StyleCommon(..)
  , Style8(..)
  , Style256(..)

  , TextSpec (..)

  -- * Basement

  -- | Ordinarily you will not need the things down here. Instead, the
  -- definitions above will give you an instance of Mod that will
  -- create the effect or color you need.

  -- ** Wrappers for effects

  , Bold (..)
  , Underline (..)
  , Flash (..)
  , Inverse (..)

  -- ** Wrappers for colors

  -- | Definitions are provided above that give you every possible
  -- color; however, these constructors are exported in case you want
  -- to make your own colors instead. Use at your own risk, as you can
  -- create non-sensical colors with this (such as 256-color colors in
  -- a 'Background8'.)
  , Background8 (..)
  , Background256 (..)
  , Foreground8 (..)
  , Foreground256 (..)

  -- ** Lenses
  , scBold
  , scUnderline
  , scFlash
  , scInverse
  , foreground8
  , background8
  , common8
  , foreground256
  , background256
  , common256
  , textSpec
  , text

  ) where


-- # Imports

import Control.Lens
import qualified Data.String as Str
import Data.Monoid (Monoid, mempty, mconcat, (<>), Last(..))
import qualified Data.Monoid as M
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import qualified Data.Text as X
import qualified System.Console.Terminfo as T
import System.IO as IO
import System.Environment as Env

--
-- Terminal definitions
--

-- | Which terminal definition to use.
data Term
  = Dumb
  -- ^ Using this terminal should always succeed. This suppresses all
  -- colors. Uesful if output is not going to a TTY, or if you just do
  -- not like colors.

  | TermName String
  -- ^ Use the terminal with this given name. You might get this from
  -- the TERM environment variable, or set it explicitly. A runtime
  -- error will result if the terminfo database does not have a
  -- definition for this terminal. If this terminal supports 256
  -- colors, then 256 colors are used. If this terminal supports less
  -- than 256 colors, but at least 8 colors, then 8 colors are
  -- used. Otherwise, no colors are used.
  deriving (Eq, Show)

-- | Gets the terminal definition from the environment. If the
-- environment does not have a TERM veriable, use 'Dumb'.
termFromEnv :: IO Term
termFromEnv = do
  t <- Env.lookupEnv "TERM"
  return $ maybe Dumb TermName t

-- | Gets the terminal definition from the environment. If the first
-- argument is True, the terminal is always obtained from the
-- environment. If it is False, the terminal is only obtained from the
-- environment if the given handle is not a terminal; otherwise, Dumb
-- is returned.
smartTermFromEnv
  :: Bool
  -- ^ Use True if the user always wants to see colors, even if
  -- standard output is not a terminal. Otherwise, use False.

  -> IO.Handle
  -- ^ Check this handle to see if it is a terminal (typically you
  -- will use stdout).

  -> IO Term
smartTermFromEnv alwaysColor h =
  if alwaysColor
  then termFromEnv
  else do
        isTerm <- IO.hIsTerminalDevice h
        if isTerm
          then termFromEnv
          else return Dumb

-- For Background8, Background256, Foreground8, Foreground256: the
-- newtype wraps a Terminfo Color. If Nothing, use the default color.

-- | Background color in an 8 color setting.
newtype Background8 = Background8
  { _unBackground8 :: Last (Maybe T.Color) }
  deriving (Eq, Show, Ord)

makeWrapped ''Background8

instance Monoid Background8 where
  mappend (Background8 b1) (Background8 b2) = Background8 $ b1 <> b2
  mempty = Background8 mempty

-- | Background color in a 256 color setting.
newtype Background256 = Background256
  { _unBackground256 :: Last (Maybe T.Color) }
  deriving (Eq, Show, Ord)

makeWrapped ''Background256

instance Monoid Background256 where
  mappend (Background256 b1) (Background256 b2)
    = Background256 $ b1 <> b2
  mempty = Background256 mempty

-- | Foreground color in an 8 color setting.
newtype Foreground8 = Foreground8
  { _unForeground8 :: Last (Maybe T.Color) }
  deriving (Eq, Show, Ord)

makeWrapped ''Foreground8

instance Monoid Foreground8 where
  mappend (Foreground8 b1) (Foreground8 b2)
    = Foreground8 $ b1 <> b2
  mempty = Foreground8 mempty

-- | Foreground color in a 256 color setting.
newtype Foreground256 = Foreground256
  { _unForeground256 :: Last (Maybe T.Color) }
  deriving (Eq, Show, Ord)

makeWrapped ''Foreground256

instance Monoid Foreground256 where
  mappend (Foreground256 b1) (Foreground256 b2)
    = Foreground256 $ b1 <> b2
  mempty = Foreground256 mempty


--
-- Effects
--
newtype Bold = Bold { _unBold :: Bool }
  deriving (Show, Eq, Ord)

makeWrapped ''Bold

newtype Underline = Underline { _unUnderline :: Bool }
  deriving (Show, Eq, Ord)

makeWrapped ''Underline

newtype Flash = Flash { _unFlash :: Bool }
  deriving (Show, Eq, Ord)

makeWrapped ''Flash

newtype Inverse = Inverse { _unInverse :: Bool }
  deriving (Show, Eq, Ord)

makeWrapped ''Inverse

--
-- Styles
--

-- | Style elements that apply in both 8 and 256 color
-- terminals. However, the elements are described separately for 8 and
-- 256 color terminals, so that the text appearance can change
-- depending on how many colors a terminal has.
data StyleCommon = StyleCommon
  { _scBold :: M.Last Bold
  , _scUnderline :: M.Last Underline
  , _scFlash :: M.Last Flash
  , _scInverse :: M.Last Inverse
  } deriving (Show, Eq, Ord)

makeLenses ''StyleCommon

instance Monoid StyleCommon where
  mempty = StyleCommon (Last Nothing) (Last Nothing)
                       (Last Nothing) (Last Nothing)
  mappend (StyleCommon b1 u1 f1 i1) (StyleCommon b2 u2 f2 i2)
    = StyleCommon (b1 <> b2) (u1 <> u2) (f1 <> f2) (i1 <> i2)

-- | Describes text appearance (foreground and background colors, as
-- well as other attributes such as bold) for an 8 color terminal.
data Style8 = Style8
  { _foreground8 :: Foreground8
  , _background8 :: Background8
  , _common8 :: StyleCommon
  } deriving (Show, Eq, Ord)

makeLenses ''Style8

instance Monoid Style8 where
  mappend (Style8 fx bx cx) (Style8 fy by cy)
    = Style8 (fx <> fy) (bx <> by) (cx <> cy)
  mempty = Style8 mempty mempty mempty

-- | Describes text appearance (foreground and background colors, as
-- well as other attributes such as bold) for a 256 color terminal.
data Style256 = Style256
  { _foreground256 :: Foreground256
  , _background256 :: Background256
  , _common256 :: StyleCommon
  } deriving (Show, Eq, Ord)

makeLenses ''Style256

instance Monoid Style256 where
  mappend (Style256 fx bx cx) (Style256 fy by cy)
    = Style256 (fx <> fy) (bx <> by) (cx <> cy)
  mempty = Style256 mempty mempty mempty

--
-- TextSpec
--

-- | The TextSpec bundles together the styles for the 8 and 256 color
-- terminals, so that the text can be portrayed on any terminal.
data TextSpec = TextSpec
  { _style8 :: Style8
  , _style256 :: Style256
  } deriving (Show, Eq, Ord)

makeLenses ''TextSpec

instance Monoid TextSpec where
  mappend (TextSpec x1 x2) (TextSpec y1 y2)
    = TextSpec (x1 <> y1) (x2 <> y2)
  mempty = TextSpec mempty mempty

--
-- Chunks
--

-- | A chunk is some textual data coupled with a description of what
-- color the text is, attributes like whether it is bold or
-- underlined, etc. The chunk knows what foreground and background
-- colors and what attributes to use for both an 8 color terminal and
-- a 256 color terminal. To change these attributes and colors, you
-- must make a new chunk.
--
-- There is no way to combine chunks. To print large numbers of
-- chunks, lazily build a list of them and then print them using
-- 'printChunks'.

data Chunk = Chunk
  { _textSpec :: TextSpec
  , _text :: Last Text
  } deriving (Eq, Show, Ord)

makeLenses ''Chunk

instance Str.IsString Chunk where
  fromString s = Chunk mempty (Last (Just (X.pack s)))

instance Monoid Chunk where
  mempty = Chunk mempty mempty
  mappend (Chunk s1 t1) (Chunk s2 t2) = Chunk (s1 <> s2) (t1 <> t2)


defaultColors :: T.Terminal -> T.TermOutput
defaultColors term =
  fromMaybe mempty (T.getCapability term T.restoreDefaultColors)

--
-- Internal
--


commonAttrs :: T.Terminal -> StyleCommon -> T.TermOutput
commonAttrs t s =
  let a = T.Attributes
        { T.standoutAttr = False
        , T.underlineAttr = maybe False _unUnderline
          $ s ^. scUnderline . unwrapped
        , T.reverseAttr = maybe False _unInverse
          $ s ^. scInverse . unwrapped
        , T.blinkAttr = maybe False _unFlash
          $ s ^. scFlash . unwrapped
        , T.dimAttr = False
        , T.boldAttr = maybe False _unBold
          $ s ^. scBold . unwrapped
        , T.invisibleAttr = False
        , T.protectedAttr = False
        }
  in case T.getCapability t (T.setAttributes) of
      Nothing -> error $ "System.Console.Rainbow: commonAttrs: "
                 ++ "capability failed; should never happen"
      Just f -> f a


-- | Gets the right set of terminal codes to apply the desired
-- highlighting, bold, underlining, etc. Be sure to apply the
-- attributes first (bold, underlining, etc) and then the
-- colors. Setting the colors first and then the attributes seems to
-- reset the colors, giving blank output.
getTermCodes
  :: T.Terminal
  -> TextSpec
  -> T.TermOutput
getTermCodes t ts = fromMaybe mempty $ do
  cols <- T.getCapability t T.termColors
  let TextSpec s8 s256 = ts
      Style8 f8 b8 c8 = s8
      Style256 f256 b256 c256 = s256
  setFg <- T.getCapability t T.setForegroundColor
  setBg <- T.getCapability t T.setBackgroundColor
  (fg, bg, cm) <- case () of
    _ | cols >= 256 -> Just $ ( f256 ^. unwrapped . unwrapped
                              , b256 ^. unwrapped . unwrapped
                              , c256)
      | cols >= 8 -> Just ( f8 ^. unwrapped . unwrapped
                         , b8 ^. unwrapped . unwrapped
                         , c8)
      | otherwise -> Nothing
  let oFg = maybe mempty (maybe mempty setFg) fg
      oBg = maybe mempty (maybe mempty setBg) bg
      oCm = commonAttrs t cm
  return $ mconcat [oCm, oFg, oBg]


hPrintChunk :: IO.Handle -> T.Terminal -> Chunk -> IO ()
hPrintChunk h t (Chunk ts x) =
  T.hRunTermOutput h t
  . mconcat
  $ [defaultColors t, codes, txt]
  where
    codes = getTermCodes t ts
    txt = T.termText . maybe "" X.unpack . getLast $ x

-- | Sends a list of chunks to the given handle for printing. Sets up
-- the terminal (this only needs to be done once.) Lazily processes
-- the list of Chunk. See 'printChunks' for notes on how many colors
-- are used.
hPrintChunks :: IO.Handle -> Term -> [Chunk] -> IO ()
hPrintChunks h t cs = do
  let setup = case t of
        Dumb -> T.setupTerm "dumb"
        TermName s -> T.setupTerm s
  term <- setup
  mapM_ (hPrintChunk h term) cs
  T.hRunTermOutput h term (defaultColors term)
  T.hRunTermOutput h term
    $ case T.getCapability term T.allAttributesOff of
        Nothing -> error $ "System.Console.Rainbow.printChunks: error: "
                   ++ "allAttributesOff failed"
        Just s -> s

-- | Sends a list of chunks to standard output for printing. Sets up
-- the terminal (this only needs to be done once.) Lazily processes
-- the list of Chunk.
--
-- Which colors are used depends upon the 'Term'. If it is 'Dumb',
-- then no colors are used on output. If the 'Term' is specified with
-- 'TermName', the UNIX terminfo library is used to determine how many
-- colors the terminal supports. If it supports at least 256 colors,
-- then 256 colors are used. If it supports at least 8 colors but less
-- than 256 colors, then 256 colors are used. Otherwise, no colors are
-- used. A runtime error will occur if the 'TermName' is not found in
-- the system terminal database.
printChunks :: Term -> [Chunk] -> IO ()
printChunks = hPrintChunks IO.stdout

-- | A convenience function for printing one chunk at a time. There
-- might be bad performance implications to using this, as the
-- terminal is reset every time (though there might not be; I haven't
-- tested it.)
putChunk :: Chunk -> IO ()
putChunk c = do
  t <- termFromEnv
  printChunks t [c]

-- | A convenience function for printing one chunk at a time. There
-- might be bad performance implications to using this, as the
-- terminal is reset every time (though there might not be; I haven't
-- tested it.) Appends a newline.
putChunkLn :: Chunk -> IO ()
putChunkLn c = putChunk c >> putStr "\n"

bold8 :: Chunk
bold8 = mempty & textSpec . style8 . common8
                 . scBold . unwrapped .~ Just (Bold True)

bold8off :: Chunk
bold8off = mempty & textSpec . style8 . common8
                    . scBold . unwrapped .~ Just (Bold False)

underline8 :: Chunk
underline8 = mempty & textSpec . style8 . common8
                 . scUnderline . unwrapped .~ Just (Underline True)

underline8off :: Chunk
underline8off = mempty & textSpec . style8 . common8
                    . scUnderline . unwrapped .~ Just (Underline False)

flash8 :: Chunk
flash8 = mempty & textSpec . style8 . common8
                 . scFlash . unwrapped .~ Just (Flash True)

flash8off :: Chunk
flash8off = mempty & textSpec . style8 . common8
                    . scFlash . unwrapped .~ Just (Flash False)

inverse8 :: Chunk
inverse8 = mempty & textSpec . style8 . common8
                 . scInverse . unwrapped .~ Just (Inverse True)

inverse8off :: Chunk
inverse8off = mempty & textSpec . style8 . common8
                    . scInverse . unwrapped .~ Just (Inverse False)

underline256 :: Chunk
underline256 = mempty & textSpec . style256 . common256
                 . scUnderline . unwrapped .~ Just (Underline True)

underline256off :: Chunk
underline256off = mempty & textSpec . style256 . common256
                    . scUnderline . unwrapped .~ Just (Underline False)

bold256 :: Chunk
bold256 = mempty & textSpec . style256 . common256
                 . scBold . unwrapped .~ Just (Bold True)

bold256off :: Chunk
bold256off = mempty & textSpec . style256 . common256
                    . scBold . unwrapped .~ Just (Bold False)

inverse256 :: Chunk
inverse256 = mempty & textSpec . style256 . common256
                 . scInverse . unwrapped .~ Just (Inverse True)

inverse256off :: Chunk
inverse256off = mempty & textSpec . style256 . common256
                    . scInverse . unwrapped .~ Just (Inverse False)

flash256 :: Chunk
flash256 = mempty & textSpec . style256 . common256
                 . scFlash . unwrapped .~ Just (Flash True)

flash256off :: Chunk
flash256off = mempty & textSpec . style256 . common256
                    . scFlash . unwrapped .~ Just (Flash False)

--
-- All
--


bold :: Chunk
bold = bold8 <> bold256

boldOff :: Chunk
boldOff = bold8off <> bold256off

inverse :: Chunk
inverse = inverse8 <> inverse256

inverseOff :: Chunk
inverseOff = inverse8off <> inverse256off

flash :: Chunk
flash = flash8 <> flash256

flashOff :: Chunk
flashOff = flash8off <> flash256off

underline :: Chunk
underline = underline8 <> underline256

underlineOff :: Chunk
underlineOff = underline8off <> underline256off

f_default :: Chunk
f_default = color8_f_default <> color256_f_default

f_black :: Chunk
f_black = color8_f_black <> color256_f_black

f_red :: Chunk
f_red = color8_f_red <> color256_f_red

f_green :: Chunk
f_green = color8_f_green <> color256_f_green

f_yellow :: Chunk
f_yellow = color8_f_yellow <> color256_f_yellow

f_blue :: Chunk
f_blue = color8_f_blue <> color256_f_blue

f_magenta :: Chunk
f_magenta = color8_f_magenta <> color256_f_magenta

f_cyan :: Chunk
f_cyan = color8_f_cyan <> color256_f_cyan

f_white :: Chunk
f_white = color8_f_white <> color256_f_white

b_default :: Chunk
b_default = color8_b_default <> color256_b_default

b_black :: Chunk
b_black = color8_b_black <> color256_b_black

b_red :: Chunk
b_red = color8_b_red <> color256_b_red

b_green :: Chunk
b_green = color8_b_green <> color256_b_green

b_yellow :: Chunk
b_yellow = color8_b_yellow <> color256_b_yellow

b_blue :: Chunk
b_blue = color8_b_blue <> color256_b_blue

b_magenta :: Chunk
b_magenta = color8_b_magenta <> color256_b_magenta

b_cyan :: Chunk
b_cyan = color8_b_cyan <> color256_b_cyan

b_white :: Chunk
b_white = color8_b_white <> color256_b_white


--
-- Color basement
--

color8_f_default :: Chunk
color8_f_default = mempty & textSpec . style8 . foreground8
                            . unwrapped . unwrapped .~ (Just Nothing)


color8_f_black :: Chunk
color8_f_black = mempty & textSpec . style8 . foreground8
  . unwrapped . unwrapped .~ (Just (Just T.Black))

color8_f_red :: Chunk
color8_f_red = mempty & textSpec . style8 . foreground8
  . unwrapped . unwrapped .~ (Just (Just T.Red))

color8_f_green :: Chunk
color8_f_green = mempty & textSpec . style8 . foreground8
  . unwrapped . unwrapped .~ (Just (Just T.Green))

color8_f_yellow :: Chunk
color8_f_yellow = mempty & textSpec . style8 . foreground8
  . unwrapped . unwrapped .~ (Just (Just T.Yellow))

color8_f_blue :: Chunk
color8_f_blue = mempty & textSpec . style8 . foreground8
  . unwrapped . unwrapped .~ (Just (Just T.Blue))

color8_f_magenta :: Chunk
color8_f_magenta = mempty & textSpec . style8 . foreground8
  . unwrapped . unwrapped .~ (Just (Just T.Magenta))

color8_f_cyan :: Chunk
color8_f_cyan = mempty & textSpec . style8 . foreground8
  . unwrapped . unwrapped .~ (Just (Just T.Cyan))

color8_f_white :: Chunk
color8_f_white = mempty & textSpec . style8 . foreground8
  . unwrapped . unwrapped .~ (Just (Just T.White))


color8_b_default :: Chunk
color8_b_default = mempty & textSpec . style8 . background8
                            . unwrapped . unwrapped .~ (Just Nothing)

color8_b_black :: Chunk
color8_b_black = mempty & textSpec . style8 . background8
  . unwrapped . unwrapped .~ (Just (Just T.Black))

color8_b_red :: Chunk
color8_b_red = mempty & textSpec . style8 . background8
  . unwrapped . unwrapped .~ (Just (Just T.Red))

color8_b_green :: Chunk
color8_b_green = mempty & textSpec . style8 . background8
  . unwrapped . unwrapped .~ (Just (Just T.Green))

color8_b_yellow :: Chunk
color8_b_yellow = mempty & textSpec . style8 . background8
  . unwrapped . unwrapped .~ (Just (Just T.Yellow))

color8_b_blue :: Chunk
color8_b_blue = mempty & textSpec . style8 . background8
  . unwrapped . unwrapped .~ (Just (Just T.Blue))

color8_b_magenta :: Chunk
color8_b_magenta = mempty & textSpec . style8 . background8
  . unwrapped . unwrapped .~ (Just (Just T.Magenta))

color8_b_cyan :: Chunk
color8_b_cyan = mempty & textSpec . style8 . background8
  . unwrapped . unwrapped .~ (Just (Just T.Cyan))

color8_b_white :: Chunk
color8_b_white = mempty & textSpec . style8 . background8
  . unwrapped . unwrapped .~ (Just (Just T.White))


color256_f_default :: Chunk
color256_f_default = mempty & textSpec . style256 . foreground256
                            . unwrapped . unwrapped .~ (Just Nothing)


color256_f_0 :: Chunk
color256_f_0 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 0)))

color256_f_black :: Chunk
color256_f_black = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 0)))

color256_f_1 :: Chunk
color256_f_1 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 1)))

color256_f_red :: Chunk
color256_f_red = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 1)))

color256_f_2 :: Chunk
color256_f_2 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 2)))

color256_f_green :: Chunk
color256_f_green = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 2)))

color256_f_3 :: Chunk
color256_f_3 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 3)))

color256_f_yellow :: Chunk
color256_f_yellow = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 3)))

color256_f_4 :: Chunk
color256_f_4 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 4)))

color256_f_blue :: Chunk
color256_f_blue = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 4)))

color256_f_5 :: Chunk
color256_f_5 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 5)))

color256_f_magenta :: Chunk
color256_f_magenta = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 5)))

color256_f_6 :: Chunk
color256_f_6 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 6)))

color256_f_cyan :: Chunk
color256_f_cyan = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 6)))

color256_f_7 :: Chunk
color256_f_7 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 7)))

color256_f_white :: Chunk
color256_f_white = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 7)))

color256_f_8 :: Chunk
color256_f_8 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 8)))

color256_f_grey :: Chunk
color256_f_grey = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 8)))

color256_f_9 :: Chunk
color256_f_9 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 9)))

color256_f_red_bright :: Chunk
color256_f_red_bright = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 9)))

color256_f_10 :: Chunk
color256_f_10 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 10)))

color256_f_green_bright :: Chunk
color256_f_green_bright = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 10)))

color256_f_11 :: Chunk
color256_f_11 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 11)))

color256_f_yellow_bright :: Chunk
color256_f_yellow_bright = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 11)))

color256_f_12 :: Chunk
color256_f_12 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 12)))

color256_f_blue_bright :: Chunk
color256_f_blue_bright = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 12)))

color256_f_13 :: Chunk
color256_f_13 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 13)))

color256_f_magenta_bright :: Chunk
color256_f_magenta_bright = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 13)))

color256_f_14 :: Chunk
color256_f_14 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 14)))

color256_f_cyan_bright :: Chunk
color256_f_cyan_bright = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 14)))

color256_f_15 :: Chunk
color256_f_15 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 15)))

color256_f_white_bright :: Chunk
color256_f_white_bright = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 15)))

color256_f_16 :: Chunk
color256_f_16 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 16)))

color256_f_17 :: Chunk
color256_f_17 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 17)))

color256_f_18 :: Chunk
color256_f_18 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 18)))

color256_f_19 :: Chunk
color256_f_19 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 19)))

color256_f_20 :: Chunk
color256_f_20 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 20)))

color256_f_21 :: Chunk
color256_f_21 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 21)))

color256_f_22 :: Chunk
color256_f_22 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 22)))

color256_f_23 :: Chunk
color256_f_23 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 23)))

color256_f_24 :: Chunk
color256_f_24 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 24)))

color256_f_25 :: Chunk
color256_f_25 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 25)))

color256_f_26 :: Chunk
color256_f_26 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 26)))

color256_f_27 :: Chunk
color256_f_27 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 27)))

color256_f_28 :: Chunk
color256_f_28 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 28)))

color256_f_29 :: Chunk
color256_f_29 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 29)))

color256_f_30 :: Chunk
color256_f_30 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 30)))

color256_f_31 :: Chunk
color256_f_31 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 31)))

color256_f_32 :: Chunk
color256_f_32 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 32)))

color256_f_33 :: Chunk
color256_f_33 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 33)))

color256_f_34 :: Chunk
color256_f_34 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 34)))

color256_f_35 :: Chunk
color256_f_35 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 35)))

color256_f_36 :: Chunk
color256_f_36 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 36)))

color256_f_37 :: Chunk
color256_f_37 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 37)))

color256_f_38 :: Chunk
color256_f_38 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 38)))

color256_f_39 :: Chunk
color256_f_39 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 39)))

color256_f_40 :: Chunk
color256_f_40 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 40)))

color256_f_41 :: Chunk
color256_f_41 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 41)))

color256_f_42 :: Chunk
color256_f_42 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 42)))

color256_f_43 :: Chunk
color256_f_43 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 43)))

color256_f_44 :: Chunk
color256_f_44 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 44)))

color256_f_45 :: Chunk
color256_f_45 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 45)))

color256_f_46 :: Chunk
color256_f_46 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 46)))

color256_f_47 :: Chunk
color256_f_47 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 47)))

color256_f_48 :: Chunk
color256_f_48 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 48)))

color256_f_49 :: Chunk
color256_f_49 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 49)))

color256_f_50 :: Chunk
color256_f_50 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 50)))

color256_f_51 :: Chunk
color256_f_51 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 51)))

color256_f_52 :: Chunk
color256_f_52 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 52)))

color256_f_53 :: Chunk
color256_f_53 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 53)))

color256_f_54 :: Chunk
color256_f_54 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 54)))

color256_f_55 :: Chunk
color256_f_55 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 55)))

color256_f_56 :: Chunk
color256_f_56 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 56)))

color256_f_57 :: Chunk
color256_f_57 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 57)))

color256_f_58 :: Chunk
color256_f_58 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 58)))

color256_f_59 :: Chunk
color256_f_59 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 59)))

color256_f_60 :: Chunk
color256_f_60 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 60)))

color256_f_61 :: Chunk
color256_f_61 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 61)))

color256_f_62 :: Chunk
color256_f_62 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 62)))

color256_f_63 :: Chunk
color256_f_63 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 63)))

color256_f_64 :: Chunk
color256_f_64 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 64)))

color256_f_65 :: Chunk
color256_f_65 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 65)))

color256_f_66 :: Chunk
color256_f_66 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 66)))

color256_f_67 :: Chunk
color256_f_67 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 67)))

color256_f_68 :: Chunk
color256_f_68 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 68)))

color256_f_69 :: Chunk
color256_f_69 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 69)))

color256_f_70 :: Chunk
color256_f_70 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 70)))

color256_f_71 :: Chunk
color256_f_71 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 71)))

color256_f_72 :: Chunk
color256_f_72 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 72)))

color256_f_73 :: Chunk
color256_f_73 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 73)))

color256_f_74 :: Chunk
color256_f_74 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 74)))

color256_f_75 :: Chunk
color256_f_75 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 75)))

color256_f_76 :: Chunk
color256_f_76 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 76)))

color256_f_77 :: Chunk
color256_f_77 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 77)))

color256_f_78 :: Chunk
color256_f_78 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 78)))

color256_f_79 :: Chunk
color256_f_79 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 79)))

color256_f_80 :: Chunk
color256_f_80 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 80)))

color256_f_81 :: Chunk
color256_f_81 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 81)))

color256_f_82 :: Chunk
color256_f_82 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 82)))

color256_f_83 :: Chunk
color256_f_83 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 83)))

color256_f_84 :: Chunk
color256_f_84 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 84)))

color256_f_85 :: Chunk
color256_f_85 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 85)))

color256_f_86 :: Chunk
color256_f_86 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 86)))

color256_f_87 :: Chunk
color256_f_87 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 87)))

color256_f_88 :: Chunk
color256_f_88 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 88)))

color256_f_89 :: Chunk
color256_f_89 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 89)))

color256_f_90 :: Chunk
color256_f_90 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 90)))

color256_f_91 :: Chunk
color256_f_91 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 91)))

color256_f_92 :: Chunk
color256_f_92 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 92)))

color256_f_93 :: Chunk
color256_f_93 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 93)))

color256_f_94 :: Chunk
color256_f_94 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 94)))

color256_f_95 :: Chunk
color256_f_95 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 95)))

color256_f_96 :: Chunk
color256_f_96 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 96)))

color256_f_97 :: Chunk
color256_f_97 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 97)))

color256_f_98 :: Chunk
color256_f_98 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 98)))

color256_f_99 :: Chunk
color256_f_99 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 99)))

color256_f_100 :: Chunk
color256_f_100 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 100)))

color256_f_101 :: Chunk
color256_f_101 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 101)))

color256_f_102 :: Chunk
color256_f_102 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 102)))

color256_f_103 :: Chunk
color256_f_103 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 103)))

color256_f_104 :: Chunk
color256_f_104 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 104)))

color256_f_105 :: Chunk
color256_f_105 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 105)))

color256_f_106 :: Chunk
color256_f_106 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 106)))

color256_f_107 :: Chunk
color256_f_107 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 107)))

color256_f_108 :: Chunk
color256_f_108 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 108)))

color256_f_109 :: Chunk
color256_f_109 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 109)))

color256_f_110 :: Chunk
color256_f_110 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 110)))

color256_f_111 :: Chunk
color256_f_111 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 111)))

color256_f_112 :: Chunk
color256_f_112 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 112)))

color256_f_113 :: Chunk
color256_f_113 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 113)))

color256_f_114 :: Chunk
color256_f_114 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 114)))

color256_f_115 :: Chunk
color256_f_115 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 115)))

color256_f_116 :: Chunk
color256_f_116 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 116)))

color256_f_117 :: Chunk
color256_f_117 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 117)))

color256_f_118 :: Chunk
color256_f_118 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 118)))

color256_f_119 :: Chunk
color256_f_119 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 119)))

color256_f_120 :: Chunk
color256_f_120 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 120)))

color256_f_121 :: Chunk
color256_f_121 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 121)))

color256_f_122 :: Chunk
color256_f_122 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 122)))

color256_f_123 :: Chunk
color256_f_123 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 123)))

color256_f_124 :: Chunk
color256_f_124 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 124)))

color256_f_125 :: Chunk
color256_f_125 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 125)))

color256_f_126 :: Chunk
color256_f_126 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 126)))

color256_f_127 :: Chunk
color256_f_127 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 127)))

color256_f_128 :: Chunk
color256_f_128 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 128)))

color256_f_129 :: Chunk
color256_f_129 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 129)))

color256_f_130 :: Chunk
color256_f_130 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 130)))

color256_f_131 :: Chunk
color256_f_131 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 131)))

color256_f_132 :: Chunk
color256_f_132 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 132)))

color256_f_133 :: Chunk
color256_f_133 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 133)))

color256_f_134 :: Chunk
color256_f_134 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 134)))

color256_f_135 :: Chunk
color256_f_135 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 135)))

color256_f_136 :: Chunk
color256_f_136 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 136)))

color256_f_137 :: Chunk
color256_f_137 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 137)))

color256_f_138 :: Chunk
color256_f_138 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 138)))

color256_f_139 :: Chunk
color256_f_139 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 139)))

color256_f_140 :: Chunk
color256_f_140 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 140)))

color256_f_141 :: Chunk
color256_f_141 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 141)))

color256_f_142 :: Chunk
color256_f_142 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 142)))

color256_f_143 :: Chunk
color256_f_143 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 143)))

color256_f_144 :: Chunk
color256_f_144 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 144)))

color256_f_145 :: Chunk
color256_f_145 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 145)))

color256_f_146 :: Chunk
color256_f_146 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 146)))

color256_f_147 :: Chunk
color256_f_147 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 147)))

color256_f_148 :: Chunk
color256_f_148 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 148)))

color256_f_149 :: Chunk
color256_f_149 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 149)))

color256_f_150 :: Chunk
color256_f_150 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 150)))

color256_f_151 :: Chunk
color256_f_151 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 151)))

color256_f_152 :: Chunk
color256_f_152 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 152)))

color256_f_153 :: Chunk
color256_f_153 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 153)))

color256_f_154 :: Chunk
color256_f_154 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 154)))

color256_f_155 :: Chunk
color256_f_155 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 155)))

color256_f_156 :: Chunk
color256_f_156 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 156)))

color256_f_157 :: Chunk
color256_f_157 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 157)))

color256_f_158 :: Chunk
color256_f_158 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 158)))

color256_f_159 :: Chunk
color256_f_159 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 159)))

color256_f_160 :: Chunk
color256_f_160 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 160)))

color256_f_161 :: Chunk
color256_f_161 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 161)))

color256_f_162 :: Chunk
color256_f_162 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 162)))

color256_f_163 :: Chunk
color256_f_163 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 163)))

color256_f_164 :: Chunk
color256_f_164 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 164)))

color256_f_165 :: Chunk
color256_f_165 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 165)))

color256_f_166 :: Chunk
color256_f_166 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 166)))

color256_f_167 :: Chunk
color256_f_167 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 167)))

color256_f_168 :: Chunk
color256_f_168 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 168)))

color256_f_169 :: Chunk
color256_f_169 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 169)))

color256_f_170 :: Chunk
color256_f_170 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 170)))

color256_f_171 :: Chunk
color256_f_171 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 171)))

color256_f_172 :: Chunk
color256_f_172 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 172)))

color256_f_173 :: Chunk
color256_f_173 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 173)))

color256_f_174 :: Chunk
color256_f_174 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 174)))

color256_f_175 :: Chunk
color256_f_175 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 175)))

color256_f_176 :: Chunk
color256_f_176 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 176)))

color256_f_177 :: Chunk
color256_f_177 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 177)))

color256_f_178 :: Chunk
color256_f_178 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 178)))

color256_f_179 :: Chunk
color256_f_179 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 179)))

color256_f_180 :: Chunk
color256_f_180 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 180)))

color256_f_181 :: Chunk
color256_f_181 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 181)))

color256_f_182 :: Chunk
color256_f_182 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 182)))

color256_f_183 :: Chunk
color256_f_183 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 183)))

color256_f_184 :: Chunk
color256_f_184 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 184)))

color256_f_185 :: Chunk
color256_f_185 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 185)))

color256_f_186 :: Chunk
color256_f_186 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 186)))

color256_f_187 :: Chunk
color256_f_187 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 187)))

color256_f_188 :: Chunk
color256_f_188 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 188)))

color256_f_189 :: Chunk
color256_f_189 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 189)))

color256_f_190 :: Chunk
color256_f_190 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 190)))

color256_f_191 :: Chunk
color256_f_191 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 191)))

color256_f_192 :: Chunk
color256_f_192 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 192)))

color256_f_193 :: Chunk
color256_f_193 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 193)))

color256_f_194 :: Chunk
color256_f_194 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 194)))

color256_f_195 :: Chunk
color256_f_195 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 195)))

color256_f_196 :: Chunk
color256_f_196 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 196)))

color256_f_197 :: Chunk
color256_f_197 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 197)))

color256_f_198 :: Chunk
color256_f_198 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 198)))

color256_f_199 :: Chunk
color256_f_199 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 199)))

color256_f_200 :: Chunk
color256_f_200 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 200)))

color256_f_201 :: Chunk
color256_f_201 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 201)))

color256_f_202 :: Chunk
color256_f_202 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 202)))

color256_f_203 :: Chunk
color256_f_203 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 203)))

color256_f_204 :: Chunk
color256_f_204 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 204)))

color256_f_205 :: Chunk
color256_f_205 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 205)))

color256_f_206 :: Chunk
color256_f_206 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 206)))

color256_f_207 :: Chunk
color256_f_207 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 207)))

color256_f_208 :: Chunk
color256_f_208 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 208)))

color256_f_209 :: Chunk
color256_f_209 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 209)))

color256_f_210 :: Chunk
color256_f_210 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 210)))

color256_f_211 :: Chunk
color256_f_211 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 211)))

color256_f_212 :: Chunk
color256_f_212 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 212)))

color256_f_213 :: Chunk
color256_f_213 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 213)))

color256_f_214 :: Chunk
color256_f_214 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 214)))

color256_f_215 :: Chunk
color256_f_215 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 215)))

color256_f_216 :: Chunk
color256_f_216 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 216)))

color256_f_217 :: Chunk
color256_f_217 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 217)))

color256_f_218 :: Chunk
color256_f_218 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 218)))

color256_f_219 :: Chunk
color256_f_219 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 219)))

color256_f_220 :: Chunk
color256_f_220 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 220)))

color256_f_221 :: Chunk
color256_f_221 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 221)))

color256_f_222 :: Chunk
color256_f_222 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 222)))

color256_f_223 :: Chunk
color256_f_223 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 223)))

color256_f_224 :: Chunk
color256_f_224 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 224)))

color256_f_225 :: Chunk
color256_f_225 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 225)))

color256_f_226 :: Chunk
color256_f_226 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 226)))

color256_f_227 :: Chunk
color256_f_227 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 227)))

color256_f_228 :: Chunk
color256_f_228 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 228)))

color256_f_229 :: Chunk
color256_f_229 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 229)))

color256_f_230 :: Chunk
color256_f_230 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 230)))

color256_f_231 :: Chunk
color256_f_231 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 231)))

color256_f_232 :: Chunk
color256_f_232 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 232)))

color256_f_233 :: Chunk
color256_f_233 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 233)))

color256_f_234 :: Chunk
color256_f_234 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 234)))

color256_f_235 :: Chunk
color256_f_235 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 235)))

color256_f_236 :: Chunk
color256_f_236 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 236)))

color256_f_237 :: Chunk
color256_f_237 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 237)))

color256_f_238 :: Chunk
color256_f_238 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 238)))

color256_f_239 :: Chunk
color256_f_239 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 239)))

color256_f_240 :: Chunk
color256_f_240 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 240)))

color256_f_241 :: Chunk
color256_f_241 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 241)))

color256_f_242 :: Chunk
color256_f_242 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 242)))

color256_f_243 :: Chunk
color256_f_243 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 243)))

color256_f_244 :: Chunk
color256_f_244 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 244)))

color256_f_245 :: Chunk
color256_f_245 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 245)))

color256_f_246 :: Chunk
color256_f_246 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 246)))

color256_f_247 :: Chunk
color256_f_247 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 247)))

color256_f_248 :: Chunk
color256_f_248 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 248)))

color256_f_249 :: Chunk
color256_f_249 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 249)))

color256_f_250 :: Chunk
color256_f_250 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 250)))

color256_f_251 :: Chunk
color256_f_251 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 251)))

color256_f_252 :: Chunk
color256_f_252 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 252)))

color256_f_253 :: Chunk
color256_f_253 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 253)))

color256_f_254 :: Chunk
color256_f_254 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 254)))

color256_f_255 :: Chunk
color256_f_255 = mempty & textSpec . style256 . foreground256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 255)))


color256_b_default :: Chunk
color256_b_default = mempty & textSpec . style256 . background256
                            . unwrapped . unwrapped .~ (Just Nothing)

color256_b_0 :: Chunk
color256_b_0 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 0)))

color256_b_black :: Chunk
color256_b_black = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 0)))

color256_b_1 :: Chunk
color256_b_1 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 1)))

color256_b_red :: Chunk
color256_b_red = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 1)))

color256_b_2 :: Chunk
color256_b_2 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 2)))

color256_b_green :: Chunk
color256_b_green = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 2)))

color256_b_3 :: Chunk
color256_b_3 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 3)))

color256_b_yellow :: Chunk
color256_b_yellow = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 3)))

color256_b_4 :: Chunk
color256_b_4 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 4)))

color256_b_blue :: Chunk
color256_b_blue = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 4)))

color256_b_5 :: Chunk
color256_b_5 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 5)))

color256_b_magenta :: Chunk
color256_b_magenta = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 5)))

color256_b_6 :: Chunk
color256_b_6 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 6)))

color256_b_cyan :: Chunk
color256_b_cyan = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 6)))

color256_b_7 :: Chunk
color256_b_7 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 7)))

color256_b_white :: Chunk
color256_b_white = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 7)))

color256_b_8 :: Chunk
color256_b_8 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 8)))

color256_b_grey :: Chunk
color256_b_grey = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 8)))

color256_b_9 :: Chunk
color256_b_9 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 9)))

color256_b_red_bright :: Chunk
color256_b_red_bright = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 9)))

color256_b_10 :: Chunk
color256_b_10 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 10)))

color256_b_green_bright :: Chunk
color256_b_green_bright = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 10)))

color256_b_11 :: Chunk
color256_b_11 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 11)))

color256_b_yellow_bright :: Chunk
color256_b_yellow_bright = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 11)))

color256_b_12 :: Chunk
color256_b_12 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 12)))

color256_b_blue_bright :: Chunk
color256_b_blue_bright = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 12)))

color256_b_13 :: Chunk
color256_b_13 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 13)))

color256_b_magenta_bright :: Chunk
color256_b_magenta_bright = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 13)))

color256_b_14 :: Chunk
color256_b_14 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 14)))

color256_b_cyan_bright :: Chunk
color256_b_cyan_bright = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 14)))

color256_b_15 :: Chunk
color256_b_15 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 15)))

color256_b_white_bright :: Chunk
color256_b_white_bright = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 15)))

color256_b_16 :: Chunk
color256_b_16 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 16)))

color256_b_17 :: Chunk
color256_b_17 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 17)))

color256_b_18 :: Chunk
color256_b_18 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 18)))

color256_b_19 :: Chunk
color256_b_19 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 19)))

color256_b_20 :: Chunk
color256_b_20 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 20)))

color256_b_21 :: Chunk
color256_b_21 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 21)))

color256_b_22 :: Chunk
color256_b_22 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 22)))

color256_b_23 :: Chunk
color256_b_23 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 23)))

color256_b_24 :: Chunk
color256_b_24 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 24)))

color256_b_25 :: Chunk
color256_b_25 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 25)))

color256_b_26 :: Chunk
color256_b_26 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 26)))

color256_b_27 :: Chunk
color256_b_27 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 27)))

color256_b_28 :: Chunk
color256_b_28 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 28)))

color256_b_29 :: Chunk
color256_b_29 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 29)))

color256_b_30 :: Chunk
color256_b_30 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 30)))

color256_b_31 :: Chunk
color256_b_31 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 31)))

color256_b_32 :: Chunk
color256_b_32 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 32)))

color256_b_33 :: Chunk
color256_b_33 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 33)))

color256_b_34 :: Chunk
color256_b_34 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 34)))

color256_b_35 :: Chunk
color256_b_35 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 35)))

color256_b_36 :: Chunk
color256_b_36 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 36)))

color256_b_37 :: Chunk
color256_b_37 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 37)))

color256_b_38 :: Chunk
color256_b_38 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 38)))

color256_b_39 :: Chunk
color256_b_39 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 39)))

color256_b_40 :: Chunk
color256_b_40 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 40)))

color256_b_41 :: Chunk
color256_b_41 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 41)))

color256_b_42 :: Chunk
color256_b_42 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 42)))

color256_b_43 :: Chunk
color256_b_43 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 43)))

color256_b_44 :: Chunk
color256_b_44 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 44)))

color256_b_45 :: Chunk
color256_b_45 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 45)))

color256_b_46 :: Chunk
color256_b_46 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 46)))

color256_b_47 :: Chunk
color256_b_47 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 47)))

color256_b_48 :: Chunk
color256_b_48 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 48)))

color256_b_49 :: Chunk
color256_b_49 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 49)))

color256_b_50 :: Chunk
color256_b_50 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 50)))

color256_b_51 :: Chunk
color256_b_51 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 51)))

color256_b_52 :: Chunk
color256_b_52 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 52)))

color256_b_53 :: Chunk
color256_b_53 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 53)))

color256_b_54 :: Chunk
color256_b_54 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 54)))

color256_b_55 :: Chunk
color256_b_55 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 55)))

color256_b_56 :: Chunk
color256_b_56 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 56)))

color256_b_57 :: Chunk
color256_b_57 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 57)))

color256_b_58 :: Chunk
color256_b_58 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 58)))

color256_b_59 :: Chunk
color256_b_59 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 59)))

color256_b_60 :: Chunk
color256_b_60 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 60)))

color256_b_61 :: Chunk
color256_b_61 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 61)))

color256_b_62 :: Chunk
color256_b_62 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 62)))

color256_b_63 :: Chunk
color256_b_63 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 63)))

color256_b_64 :: Chunk
color256_b_64 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 64)))

color256_b_65 :: Chunk
color256_b_65 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 65)))

color256_b_66 :: Chunk
color256_b_66 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 66)))

color256_b_67 :: Chunk
color256_b_67 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 67)))

color256_b_68 :: Chunk
color256_b_68 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 68)))

color256_b_69 :: Chunk
color256_b_69 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 69)))

color256_b_70 :: Chunk
color256_b_70 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 70)))

color256_b_71 :: Chunk
color256_b_71 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 71)))

color256_b_72 :: Chunk
color256_b_72 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 72)))

color256_b_73 :: Chunk
color256_b_73 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 73)))

color256_b_74 :: Chunk
color256_b_74 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 74)))

color256_b_75 :: Chunk
color256_b_75 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 75)))

color256_b_76 :: Chunk
color256_b_76 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 76)))

color256_b_77 :: Chunk
color256_b_77 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 77)))

color256_b_78 :: Chunk
color256_b_78 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 78)))

color256_b_79 :: Chunk
color256_b_79 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 79)))

color256_b_80 :: Chunk
color256_b_80 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 80)))

color256_b_81 :: Chunk
color256_b_81 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 81)))

color256_b_82 :: Chunk
color256_b_82 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 82)))

color256_b_83 :: Chunk
color256_b_83 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 83)))

color256_b_84 :: Chunk
color256_b_84 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 84)))

color256_b_85 :: Chunk
color256_b_85 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 85)))

color256_b_86 :: Chunk
color256_b_86 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 86)))

color256_b_87 :: Chunk
color256_b_87 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 87)))

color256_b_88 :: Chunk
color256_b_88 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 88)))

color256_b_89 :: Chunk
color256_b_89 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 89)))

color256_b_90 :: Chunk
color256_b_90 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 90)))

color256_b_91 :: Chunk
color256_b_91 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 91)))

color256_b_92 :: Chunk
color256_b_92 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 92)))

color256_b_93 :: Chunk
color256_b_93 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 93)))

color256_b_94 :: Chunk
color256_b_94 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 94)))

color256_b_95 :: Chunk
color256_b_95 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 95)))

color256_b_96 :: Chunk
color256_b_96 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 96)))

color256_b_97 :: Chunk
color256_b_97 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 97)))

color256_b_98 :: Chunk
color256_b_98 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 98)))

color256_b_99 :: Chunk
color256_b_99 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 99)))

color256_b_100 :: Chunk
color256_b_100 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 100)))

color256_b_101 :: Chunk
color256_b_101 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 101)))

color256_b_102 :: Chunk
color256_b_102 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 102)))

color256_b_103 :: Chunk
color256_b_103 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 103)))

color256_b_104 :: Chunk
color256_b_104 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 104)))

color256_b_105 :: Chunk
color256_b_105 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 105)))

color256_b_106 :: Chunk
color256_b_106 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 106)))

color256_b_107 :: Chunk
color256_b_107 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 107)))

color256_b_108 :: Chunk
color256_b_108 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 108)))

color256_b_109 :: Chunk
color256_b_109 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 109)))

color256_b_110 :: Chunk
color256_b_110 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 110)))

color256_b_111 :: Chunk
color256_b_111 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 111)))

color256_b_112 :: Chunk
color256_b_112 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 112)))

color256_b_113 :: Chunk
color256_b_113 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 113)))

color256_b_114 :: Chunk
color256_b_114 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 114)))

color256_b_115 :: Chunk
color256_b_115 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 115)))

color256_b_116 :: Chunk
color256_b_116 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 116)))

color256_b_117 :: Chunk
color256_b_117 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 117)))

color256_b_118 :: Chunk
color256_b_118 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 118)))

color256_b_119 :: Chunk
color256_b_119 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 119)))

color256_b_120 :: Chunk
color256_b_120 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 120)))

color256_b_121 :: Chunk
color256_b_121 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 121)))

color256_b_122 :: Chunk
color256_b_122 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 122)))

color256_b_123 :: Chunk
color256_b_123 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 123)))

color256_b_124 :: Chunk
color256_b_124 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 124)))

color256_b_125 :: Chunk
color256_b_125 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 125)))

color256_b_126 :: Chunk
color256_b_126 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 126)))

color256_b_127 :: Chunk
color256_b_127 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 127)))

color256_b_128 :: Chunk
color256_b_128 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 128)))

color256_b_129 :: Chunk
color256_b_129 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 129)))

color256_b_130 :: Chunk
color256_b_130 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 130)))

color256_b_131 :: Chunk
color256_b_131 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 131)))

color256_b_132 :: Chunk
color256_b_132 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 132)))

color256_b_133 :: Chunk
color256_b_133 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 133)))

color256_b_134 :: Chunk
color256_b_134 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 134)))

color256_b_135 :: Chunk
color256_b_135 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 135)))

color256_b_136 :: Chunk
color256_b_136 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 136)))

color256_b_137 :: Chunk
color256_b_137 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 137)))

color256_b_138 :: Chunk
color256_b_138 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 138)))

color256_b_139 :: Chunk
color256_b_139 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 139)))

color256_b_140 :: Chunk
color256_b_140 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 140)))

color256_b_141 :: Chunk
color256_b_141 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 141)))

color256_b_142 :: Chunk
color256_b_142 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 142)))

color256_b_143 :: Chunk
color256_b_143 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 143)))

color256_b_144 :: Chunk
color256_b_144 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 144)))

color256_b_145 :: Chunk
color256_b_145 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 145)))

color256_b_146 :: Chunk
color256_b_146 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 146)))

color256_b_147 :: Chunk
color256_b_147 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 147)))

color256_b_148 :: Chunk
color256_b_148 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 148)))

color256_b_149 :: Chunk
color256_b_149 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 149)))

color256_b_150 :: Chunk
color256_b_150 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 150)))

color256_b_151 :: Chunk
color256_b_151 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 151)))

color256_b_152 :: Chunk
color256_b_152 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 152)))

color256_b_153 :: Chunk
color256_b_153 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 153)))

color256_b_154 :: Chunk
color256_b_154 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 154)))

color256_b_155 :: Chunk
color256_b_155 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 155)))

color256_b_156 :: Chunk
color256_b_156 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 156)))

color256_b_157 :: Chunk
color256_b_157 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 157)))

color256_b_158 :: Chunk
color256_b_158 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 158)))

color256_b_159 :: Chunk
color256_b_159 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 159)))

color256_b_160 :: Chunk
color256_b_160 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 160)))

color256_b_161 :: Chunk
color256_b_161 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 161)))

color256_b_162 :: Chunk
color256_b_162 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 162)))

color256_b_163 :: Chunk
color256_b_163 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 163)))

color256_b_164 :: Chunk
color256_b_164 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 164)))

color256_b_165 :: Chunk
color256_b_165 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 165)))

color256_b_166 :: Chunk
color256_b_166 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 166)))

color256_b_167 :: Chunk
color256_b_167 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 167)))

color256_b_168 :: Chunk
color256_b_168 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 168)))

color256_b_169 :: Chunk
color256_b_169 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 169)))

color256_b_170 :: Chunk
color256_b_170 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 170)))

color256_b_171 :: Chunk
color256_b_171 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 171)))

color256_b_172 :: Chunk
color256_b_172 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 172)))

color256_b_173 :: Chunk
color256_b_173 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 173)))

color256_b_174 :: Chunk
color256_b_174 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 174)))

color256_b_175 :: Chunk
color256_b_175 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 175)))

color256_b_176 :: Chunk
color256_b_176 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 176)))

color256_b_177 :: Chunk
color256_b_177 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 177)))

color256_b_178 :: Chunk
color256_b_178 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 178)))

color256_b_179 :: Chunk
color256_b_179 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 179)))

color256_b_180 :: Chunk
color256_b_180 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 180)))

color256_b_181 :: Chunk
color256_b_181 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 181)))

color256_b_182 :: Chunk
color256_b_182 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 182)))

color256_b_183 :: Chunk
color256_b_183 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 183)))

color256_b_184 :: Chunk
color256_b_184 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 184)))

color256_b_185 :: Chunk
color256_b_185 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 185)))

color256_b_186 :: Chunk
color256_b_186 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 186)))

color256_b_187 :: Chunk
color256_b_187 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 187)))

color256_b_188 :: Chunk
color256_b_188 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 188)))

color256_b_189 :: Chunk
color256_b_189 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 189)))

color256_b_190 :: Chunk
color256_b_190 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 190)))

color256_b_191 :: Chunk
color256_b_191 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 191)))

color256_b_192 :: Chunk
color256_b_192 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 192)))

color256_b_193 :: Chunk
color256_b_193 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 193)))

color256_b_194 :: Chunk
color256_b_194 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 194)))

color256_b_195 :: Chunk
color256_b_195 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 195)))

color256_b_196 :: Chunk
color256_b_196 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 196)))

color256_b_197 :: Chunk
color256_b_197 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 197)))

color256_b_198 :: Chunk
color256_b_198 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 198)))

color256_b_199 :: Chunk
color256_b_199 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 199)))

color256_b_200 :: Chunk
color256_b_200 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 200)))

color256_b_201 :: Chunk
color256_b_201 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 201)))

color256_b_202 :: Chunk
color256_b_202 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 202)))

color256_b_203 :: Chunk
color256_b_203 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 203)))

color256_b_204 :: Chunk
color256_b_204 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 204)))

color256_b_205 :: Chunk
color256_b_205 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 205)))

color256_b_206 :: Chunk
color256_b_206 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 206)))

color256_b_207 :: Chunk
color256_b_207 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 207)))

color256_b_208 :: Chunk
color256_b_208 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 208)))

color256_b_209 :: Chunk
color256_b_209 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 209)))

color256_b_210 :: Chunk
color256_b_210 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 210)))

color256_b_211 :: Chunk
color256_b_211 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 211)))

color256_b_212 :: Chunk
color256_b_212 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 212)))

color256_b_213 :: Chunk
color256_b_213 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 213)))

color256_b_214 :: Chunk
color256_b_214 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 214)))

color256_b_215 :: Chunk
color256_b_215 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 215)))

color256_b_216 :: Chunk
color256_b_216 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 216)))

color256_b_217 :: Chunk
color256_b_217 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 217)))

color256_b_218 :: Chunk
color256_b_218 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 218)))

color256_b_219 :: Chunk
color256_b_219 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 219)))

color256_b_220 :: Chunk
color256_b_220 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 220)))

color256_b_221 :: Chunk
color256_b_221 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 221)))

color256_b_222 :: Chunk
color256_b_222 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 222)))

color256_b_223 :: Chunk
color256_b_223 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 223)))

color256_b_224 :: Chunk
color256_b_224 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 224)))

color256_b_225 :: Chunk
color256_b_225 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 225)))

color256_b_226 :: Chunk
color256_b_226 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 226)))

color256_b_227 :: Chunk
color256_b_227 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 227)))

color256_b_228 :: Chunk
color256_b_228 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 228)))

color256_b_229 :: Chunk
color256_b_229 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 229)))

color256_b_230 :: Chunk
color256_b_230 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 230)))

color256_b_231 :: Chunk
color256_b_231 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 231)))

color256_b_232 :: Chunk
color256_b_232 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 232)))

color256_b_233 :: Chunk
color256_b_233 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 233)))

color256_b_234 :: Chunk
color256_b_234 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 234)))

color256_b_235 :: Chunk
color256_b_235 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 235)))

color256_b_236 :: Chunk
color256_b_236 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 236)))

color256_b_237 :: Chunk
color256_b_237 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 237)))

color256_b_238 :: Chunk
color256_b_238 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 238)))

color256_b_239 :: Chunk
color256_b_239 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 239)))

color256_b_240 :: Chunk
color256_b_240 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 240)))

color256_b_241 :: Chunk
color256_b_241 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 241)))

color256_b_242 :: Chunk
color256_b_242 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 242)))

color256_b_243 :: Chunk
color256_b_243 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 243)))

color256_b_244 :: Chunk
color256_b_244 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 244)))

color256_b_245 :: Chunk
color256_b_245 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 245)))

color256_b_246 :: Chunk
color256_b_246 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 246)))

color256_b_247 :: Chunk
color256_b_247 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 247)))

color256_b_248 :: Chunk
color256_b_248 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 248)))

color256_b_249 :: Chunk
color256_b_249 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 249)))

color256_b_250 :: Chunk
color256_b_250 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 250)))

color256_b_251 :: Chunk
color256_b_251 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 251)))

color256_b_252 :: Chunk
color256_b_252 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 252)))

color256_b_253 :: Chunk
color256_b_253 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 253)))

color256_b_254 :: Chunk
color256_b_254 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 254)))

color256_b_255 :: Chunk
color256_b_255 = mempty & textSpec . style256 . background256
  . unwrapped . unwrapped .~ (Just (Just (T.ColorNumber 255)))

