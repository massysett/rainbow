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
-- use '.+.' to combine different modifiers to change how the Chunk is
-- rendered. Here's an example:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > blueHello :: Chunk
-- > blueHello = plain "Hello world!" .+. color8_f_blue .+. color8_b_white
-- >             .+. color256_f_blue .+. color256_b_blue
-- >             .+. underline8 .+. underline256

module System.Console.Rainbow (
  -- * Colors
    Term(..)
  , Background8
  , Background256
  , Foreground8
  , Foreground256

  -- * Chunks
  , Chunk (chunkTextSpec, chunkText)
  , chunk
  , plain
  , Width(Width, unWidth)
  , chunkWidth
  , printChunks

  -- * Mod
  , Mod(..)
  , ChangeText(..)
  , (.+.)

  -- * Effects
  , Bold (Bold, unBold)
  , Underline (Underline, unUnderline)
  , Flash (Flash, unFlash)
  , Inverse (Inverse, unInverse)

  , Bold8 (Bold8, unBold8)
  , bold8, bold8off

  , Underline8 (Underline8, unUnderline8)
  , underline8, underline8off

  , Flash8 (Flash8, unFlash8)
  , flash8, flash8off

  , Inverse8 (Inverse8, unInverse8)
  , inverse8, inverse8off

  , Bold256 (Bold256, unBold256)
  , bold256, bold256off

  , Underline256 (Underline256, unUnderline256)
  , underline256, underline256off

  , Flash256 (Flash256, unFlash256)
  , flash256, flash256off

  , Inverse256 (Inverse256, unInverse256)
  , inverse256, inverse256off


  -- * Style and TextSpec

  -- | A style is a bundle of attributes that describes text
  -- attributes, such as its color and whether it is bold.
  , StyleCommon (StyleCommon, bold, underline, flash, inverse)
  , Style8 (Style8, foreground8, background8, common8)
  , Style256 (Style256, foreground256, background256, common256)
  , defaultStyleCommon
  , defaultStyle8
  , defaultStyle256

  , TextSpec (TextSpec, style8, style256)
  , defaultTextSpec

  -- * Color switchers
  , switchForeground
  , switchBackground

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

  ) where


import Data.Monoid (Monoid, mempty, mappend, mconcat)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import qualified Data.Text as X
import qualified System.Console.Terminfo as T

--
-- Colors
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


--
-- Mod
--

class Mod a where
  changeChunk :: Chunk -> a -> Chunk

instance Mod Text where
  changeChunk (Chunk ts _) t' = Chunk ts t'

newtype ChangeText = ChangeText { unChangeText :: Text -> Text }

instance Mod ChangeText where
  changeChunk (Chunk ts t) (ChangeText f) = Chunk ts (f t)

(.+.) :: Mod a => Chunk -> a -> Chunk
(.+.) = changeChunk
infixl 6 .+.

-- For Background8, Background256, Foreground8, Foreground256: the
-- newtype wraps a Terminfo Color. If Nothing, use the default color.

-- | Background color in an 8 color setting.
newtype Background8 = Background8 { unBackground8 :: Maybe T.Color }
  deriving (Eq, Show, Ord)

instance Mod Background8 where
  changeChunk (Chunk ts t) b8 =
    Chunk (ts { style8 = (style8 ts) { background8 = b8 } } ) t

-- | Background color in a 256 color setting.
newtype Background256 = Background256 { unBackground256 :: Maybe T.Color }
  deriving (Eq, Show, Ord)

instance Mod Background256 where
  changeChunk (Chunk ts t) b256 =
    Chunk (ts { style256 = (style256 ts) { background256 = b256 } } ) t

-- | Foreground color in an 8 color setting.
newtype Foreground8 = Foreground8 { unForeground8 :: Maybe T.Color }
  deriving (Eq, Show, Ord)

instance Mod Foreground8 where
  changeChunk (Chunk ts t) f8 =
    Chunk (ts { style8 = (style8 ts) { foreground8 = f8 } } ) t

-- | Foreground color in a 256 color setting.
newtype Foreground256 = Foreground256 { unForeground256 :: Maybe T.Color }
  deriving (Eq, Show, Ord)

instance Mod Foreground256 where
  changeChunk (Chunk ts t) f256 =
    Chunk (ts { style256 = (style256 ts) { foreground256 = f256 } } ) t


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
  { chunkTextSpec :: TextSpec
  , chunkText :: Text
  } deriving (Eq, Show, Ord)

-- | Makes new Chunks.
chunk :: TextSpec -> Text -> Chunk
chunk = Chunk

-- | Makes a plain Chunk; that is, one with a defaultTextSpec.
plain :: Text -> Chunk
plain = chunk defaultTextSpec

-- | How wide the text of a chunk is.
newtype Width = Width { unWidth :: Int }
                deriving (Show, Eq, Ord)

instance Monoid Width where
  mempty = Width 0
  mappend (Width w1) (Width w2) = Width $ w1 + w2

chunkWidth :: Chunk -> Width
chunkWidth (Chunk _ t) = Width . X.length $ t

-- | Sends a list of chunks to standard output for printing. Sets up
-- the terminal (this only needs to be done once.) Lazily processes
-- the list of Chunk.
printChunks :: Term -> [Chunk] -> IO ()
printChunks t cs = do
  let setup = case t of
        Dumb -> T.setupTerm "dumb"
        TermName s -> T.setupTerm s
  term <- setup
  mapM_ (printChunk term) cs
  T.runTermOutput term (defaultColors term)
  T.runTermOutput term
    $ case T.getCapability term T.allAttributesOff of
        Nothing -> error $ "System.Console.Rainbow.printChunks: error: "
                   ++ "allAttributesOff failed"
        Just s -> s

printChunk :: T.Terminal -> Chunk -> IO ()
printChunk t (Chunk ts x) =
  T.runTermOutput t
  . mconcat
  $ [defaultColors t, codes, txt]
  where
    codes = getTermCodes t ts
    txt = T.termText . X.unpack $ x

defaultColors :: T.Terminal -> T.TermOutput
defaultColors term =
  fromMaybe mempty (T.getCapability term T.restoreDefaultColors)

--
-- Effects
--
newtype Bold = Bold { unBold :: Bool }
  deriving (Show, Eq, Ord)

newtype Underline = Underline { unUnderline :: Bool }
  deriving (Show, Eq, Ord)

newtype Flash = Flash { unFlash :: Bool }
  deriving (Show, Eq, Ord)

newtype Inverse = Inverse { unInverse :: Bool }
  deriving (Show, Eq, Ord)

newtype Bold8 = Bold8 { unBold8 :: Bold }
  deriving (Show, Eq, Ord)

bold8 :: Bold8
bold8 = Bold8 (Bold True)

bold8off :: Bold8
bold8off = Bold8 (Bold False)

instance Mod Bold8 where
  changeChunk (Chunk ts t) (Bold8 b) =
    let c8 = common8 . style8 $ ts
        c8' = c8 { bold = b }
        ts' = ts { style8 = (style8 ts) { common8 = c8' } }
    in Chunk ts' t

newtype Underline8 = Underline8 { unUnderline8 :: Underline }
  deriving (Show, Eq, Ord)

underline8 :: Underline8
underline8 = Underline8 (Underline True)

underline8off :: Underline8
underline8off = Underline8 (Underline False)

instance Mod Underline8 where
  changeChunk (Chunk ts t) (Underline8 u) =
    let c8 = common8 . style8 $ ts
        c8' = c8 { underline = u }
        ts' = ts { style8 = (style8 ts) { common8 = c8' } }
    in Chunk ts' t

newtype Flash8 = Flash8 { unFlash8 :: Flash }
  deriving (Show, Eq, Ord)

flash8 :: Flash8
flash8 = Flash8 (Flash True)

flash8off :: Flash8
flash8off = Flash8 (Flash False)

instance Mod Flash8 where
  changeChunk (Chunk ts t) (Flash8 f) =
    let c8 = common8 . style8 $ ts
        c8' = c8 { flash = f }
        ts' = ts { style8 = (style8 ts) { common8 = c8' } }
    in Chunk ts' t

newtype Inverse8 = Inverse8 { unInverse8 :: Inverse }
  deriving (Show, Eq, Ord)

inverse8 :: Inverse8
inverse8 = Inverse8 (Inverse True)

inverse8off :: Inverse8
inverse8off = Inverse8 (Inverse False)

instance Mod Inverse8 where
  changeChunk (Chunk ts t) (Inverse8 i) =
    let c8 = common8 . style8 $ ts
        c8' = c8 { inverse = i }
        ts' = ts { style8 = (style8 ts) { common8 = c8' } }
    in Chunk ts' t

newtype Bold256 = Bold256 { unBold256 :: Bold }
  deriving (Show, Eq, Ord)

bold256 :: Bold256
bold256 = Bold256 (Bold True)

bold256off :: Bold256
bold256off = Bold256 (Bold False)

instance Mod Bold256 where
  changeChunk (Chunk ts t) (Bold256 b) =
    let c256 = common256 . style256 $ ts
        c256' = c256 { bold = b }
        ts' = ts { style256 = (style256 ts) { common256 = c256' } }
    in Chunk ts' t

newtype Underline256 = Underline256 { unUnderline256 :: Underline }
  deriving (Show, Eq, Ord)

underline256 :: Underline256
underline256 = Underline256 (Underline True)

underline256off :: Underline256
underline256off = Underline256 (Underline False)

instance Mod Underline256 where
  changeChunk (Chunk ts t) (Underline256 u) =
    let c256 = common256 . style256 $ ts
        c256' = c256 { underline = u }
        ts' = ts { style256 = (style256 ts) { common256 = c256' } }
    in Chunk ts' t

newtype Flash256 = Flash256 { unFlash256 :: Flash }
  deriving (Show, Eq, Ord)

flash256 :: Flash256
flash256 = Flash256 (Flash True)

flash256off :: Flash256
flash256off = Flash256 (Flash False)

instance Mod Flash256 where
  changeChunk (Chunk ts t) (Flash256 f) =
    let c256 = common256 . style256 $ ts
        c256' = c256 { flash = f }
        ts' = ts { style256 = (style256 ts) { common256 = c256' } }
    in Chunk ts' t

newtype Inverse256 = Inverse256 { unInverse256 :: Inverse }
  deriving (Show, Eq, Ord)

inverse256 :: Inverse256
inverse256 = Inverse256 (Inverse True)

inverse256off :: Inverse256
inverse256off = Inverse256 (Inverse False)

instance Mod Inverse256 where
  changeChunk (Chunk ts t) (Inverse256 i) =
    let c256 = common256 . style256 $ ts
        c256' = c256 { inverse = i }
        ts' = ts { style256 = (style256 ts) { common256 = c256' } }
    in Chunk ts' t


--
-- Styles
--

-- | Style elements that apply in both 8 and 256 color
-- terminals. However, the elements are described separately for 8 and
-- 256 color terminals, so that the text appearance can change
-- depending on how many colors a terminal has.
data StyleCommon = StyleCommon
  { bold :: Bold
  , underline :: Underline
  , flash :: Flash
  , inverse :: Inverse
  } deriving (Show, Eq, Ord)

-- | Describes text appearance (foreground and background colors, as
-- well as other attributes such as bold) for an 8 color terminal.
data Style8 = Style8
  { foreground8 :: Foreground8
  , background8 :: Background8
  , common8 :: StyleCommon
  } deriving (Show, Eq, Ord)

-- | Describes text appearance (foreground and background colors, as
-- well as other attributes such as bold) for a 256 color terminal.
data Style256 = Style256
  { foreground256 :: Foreground256
  , background256 :: Background256
  , common256 :: StyleCommon
  } deriving (Show, Eq, Ord)

-- | Has all bold, flash, underline, and inverse turned off.
defaultStyleCommon :: StyleCommon
defaultStyleCommon = StyleCommon
  { bold = Bold False
  , underline = Underline False
  , flash = Flash False
  , inverse = Inverse False
  }

-- | Uses the default terminal colors (which will vary depending on
-- the terminal).
defaultStyle8 :: Style8
defaultStyle8 = Style8
  { foreground8 = color8_f_default
  , background8 = color8_b_default
  , common8 = defaultStyleCommon
  }

-- | Uses the default terminal colors (which will vary depending on
-- the terminal).
defaultStyle256 :: Style256
defaultStyle256 = Style256
  { foreground256 = color256_f_default
  , background256 = color256_b_default
  , common256 = defaultStyleCommon
  }

--
-- TextSpec
--

-- | The TextSpec bundles together the styles for the 8 and 256 color
-- terminals, so that the text can be portrayed on any terminal.
data TextSpec = TextSpec
  { style8 :: Style8
  , style256 :: Style256
  } deriving (Show, Eq, Ord)

-- | A TextSpec with the default colors on 8 and 256 color terminals,
-- with all attributes turned off.
defaultTextSpec :: TextSpec
defaultTextSpec = TextSpec
  { style8 = defaultStyle8
  , style256 = defaultStyle256
  }

-- | Switch the foreground colors for new ones.
switchForeground
  :: Foreground8
  -> Foreground256
  -> TextSpec
  -> TextSpec
switchForeground c8 c256 ts = ts' where
  ts' = TextSpec s8' s256'
  s8' = (style8 ts) { foreground8 = c8 }
  s256' = (style256 ts) { foreground256 = c256 }

-- | Switch the background colors for new ones.
switchBackground
  :: Background8
  -> Background256
  -> TextSpec
  -> TextSpec
switchBackground c8 c256 ts = ts' where
  ts' = TextSpec s8' s256'
  s8' = (style8 ts) { background8 = c8 }
  s256' = (style256 ts) { background256 = c256 }

--
-- Internal
--

commonAttrs :: T.Terminal -> StyleCommon -> T.TermOutput
commonAttrs t s =
  let a = T.Attributes
        { T.standoutAttr = False
        , T.underlineAttr = unUnderline . underline $ s
        , T.reverseAttr = unInverse . inverse $ s
        , T.blinkAttr = unFlash . flash $ s
        , T.dimAttr = False
        , T.boldAttr = unBold . bold $ s
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
    _ | cols >= 256 -> Just $ ( unForeground256 f256
                              , unBackground256 b256
                              , c256 )
      | cols >= 8 -> Just ( unForeground8 f8
                         , unBackground8 b8
                         , c8)
      | otherwise -> Nothing
  let oFg = maybe mempty setFg fg
      oBg = maybe mempty setBg bg
      oCm = commonAttrs t cm
  return $ mconcat [oCm, oFg, oBg]


--
-- Color basement
--
color8_f_default :: Foreground8
color8_f_default = Foreground8 Nothing

color8_f_black :: Foreground8
color8_f_black = Foreground8 $ Just T.Black

color8_f_red :: Foreground8
color8_f_red = Foreground8 $ Just T.Red

color8_f_green :: Foreground8
color8_f_green = Foreground8 $ Just T.Green

color8_f_yellow :: Foreground8
color8_f_yellow = Foreground8 $ Just T.Yellow

color8_f_blue :: Foreground8
color8_f_blue = Foreground8 $ Just T.Blue

color8_f_magenta :: Foreground8
color8_f_magenta = Foreground8 $ Just T.Magenta

color8_f_cyan :: Foreground8
color8_f_cyan = Foreground8 $ Just T.Cyan

color8_f_white :: Foreground8
color8_f_white = Foreground8 $ Just T.White

color8_b_default :: Background8
color8_b_default = Background8 Nothing

color8_b_black :: Background8
color8_b_black = Background8 $ Just T.Black

color8_b_red :: Background8
color8_b_red = Background8 $ Just T.Red

color8_b_green :: Background8
color8_b_green = Background8 $ Just T.Green

color8_b_yellow :: Background8
color8_b_yellow = Background8 $ Just T.Yellow

color8_b_blue :: Background8
color8_b_blue = Background8 $ Just T.Blue

color8_b_magenta :: Background8
color8_b_magenta = Background8 $ Just T.Magenta

color8_b_cyan :: Background8
color8_b_cyan = Background8 $ Just T.Cyan

color8_b_white :: Background8
color8_b_white = Background8 $ Just T.White

color256_f_default :: Foreground256
color256_f_default = Foreground256 Nothing

color256_f_0 :: Foreground256
color256_f_0 = Foreground256 (Just (T.ColorNumber 0))

color256_f_black :: Foreground256
color256_f_black = Foreground256 (Just (T.ColorNumber 0))

color256_f_1 :: Foreground256
color256_f_1 = Foreground256 (Just (T.ColorNumber 1))

color256_f_red :: Foreground256
color256_f_red = Foreground256 (Just (T.ColorNumber 1))

color256_f_2 :: Foreground256
color256_f_2 = Foreground256 (Just (T.ColorNumber 2))

color256_f_green :: Foreground256
color256_f_green = Foreground256 (Just (T.ColorNumber 2))

color256_f_3 :: Foreground256
color256_f_3 = Foreground256 (Just (T.ColorNumber 3))

color256_f_yellow :: Foreground256
color256_f_yellow = Foreground256 (Just (T.ColorNumber 3))

color256_f_4 :: Foreground256
color256_f_4 = Foreground256 (Just (T.ColorNumber 4))

color256_f_blue :: Foreground256
color256_f_blue = Foreground256 (Just (T.ColorNumber 4))

color256_f_5 :: Foreground256
color256_f_5 = Foreground256 (Just (T.ColorNumber 5))

color256_f_magenta :: Foreground256
color256_f_magenta = Foreground256 (Just (T.ColorNumber 5))

color256_f_6 :: Foreground256
color256_f_6 = Foreground256 (Just (T.ColorNumber 6))

color256_f_cyan :: Foreground256
color256_f_cyan = Foreground256 (Just (T.ColorNumber 6))

color256_f_7 :: Foreground256
color256_f_7 = Foreground256 (Just (T.ColorNumber 7))

color256_f_white :: Foreground256
color256_f_white = Foreground256 (Just (T.ColorNumber 7))

color256_f_8 :: Foreground256
color256_f_8 = Foreground256 (Just (T.ColorNumber 8))

color256_f_grey :: Foreground256
color256_f_grey = Foreground256 (Just (T.ColorNumber 8))

color256_f_9 :: Foreground256
color256_f_9 = Foreground256 (Just (T.ColorNumber 9))

color256_f_red_bright :: Foreground256
color256_f_red_bright = Foreground256 (Just (T.ColorNumber 9))

color256_f_10 :: Foreground256
color256_f_10 = Foreground256 (Just (T.ColorNumber 10))

color256_f_green_bright :: Foreground256
color256_f_green_bright = Foreground256 (Just (T.ColorNumber 10))

color256_f_11 :: Foreground256
color256_f_11 = Foreground256 (Just (T.ColorNumber 11))

color256_f_yellow_bright :: Foreground256
color256_f_yellow_bright = Foreground256 (Just (T.ColorNumber 11))

color256_f_12 :: Foreground256
color256_f_12 = Foreground256 (Just (T.ColorNumber 12))

color256_f_blue_bright :: Foreground256
color256_f_blue_bright = Foreground256 (Just (T.ColorNumber 12))

color256_f_13 :: Foreground256
color256_f_13 = Foreground256 (Just (T.ColorNumber 13))

color256_f_magenta_bright :: Foreground256
color256_f_magenta_bright = Foreground256 (Just (T.ColorNumber 13))

color256_f_14 :: Foreground256
color256_f_14 = Foreground256 (Just (T.ColorNumber 14))

color256_f_cyan_bright :: Foreground256
color256_f_cyan_bright = Foreground256 (Just (T.ColorNumber 14))

color256_f_15 :: Foreground256
color256_f_15 = Foreground256 (Just (T.ColorNumber 15))

color256_f_white_bright :: Foreground256
color256_f_white_bright = Foreground256 (Just (T.ColorNumber 15))

color256_f_16 :: Foreground256
color256_f_16 = Foreground256 (Just (T.ColorNumber 16))

color256_f_17 :: Foreground256
color256_f_17 = Foreground256 (Just (T.ColorNumber 17))

color256_f_18 :: Foreground256
color256_f_18 = Foreground256 (Just (T.ColorNumber 18))

color256_f_19 :: Foreground256
color256_f_19 = Foreground256 (Just (T.ColorNumber 19))

color256_f_20 :: Foreground256
color256_f_20 = Foreground256 (Just (T.ColorNumber 20))

color256_f_21 :: Foreground256
color256_f_21 = Foreground256 (Just (T.ColorNumber 21))

color256_f_22 :: Foreground256
color256_f_22 = Foreground256 (Just (T.ColorNumber 22))

color256_f_23 :: Foreground256
color256_f_23 = Foreground256 (Just (T.ColorNumber 23))

color256_f_24 :: Foreground256
color256_f_24 = Foreground256 (Just (T.ColorNumber 24))

color256_f_25 :: Foreground256
color256_f_25 = Foreground256 (Just (T.ColorNumber 25))

color256_f_26 :: Foreground256
color256_f_26 = Foreground256 (Just (T.ColorNumber 26))

color256_f_27 :: Foreground256
color256_f_27 = Foreground256 (Just (T.ColorNumber 27))

color256_f_28 :: Foreground256
color256_f_28 = Foreground256 (Just (T.ColorNumber 28))

color256_f_29 :: Foreground256
color256_f_29 = Foreground256 (Just (T.ColorNumber 29))

color256_f_30 :: Foreground256
color256_f_30 = Foreground256 (Just (T.ColorNumber 30))

color256_f_31 :: Foreground256
color256_f_31 = Foreground256 (Just (T.ColorNumber 31))

color256_f_32 :: Foreground256
color256_f_32 = Foreground256 (Just (T.ColorNumber 32))

color256_f_33 :: Foreground256
color256_f_33 = Foreground256 (Just (T.ColorNumber 33))

color256_f_34 :: Foreground256
color256_f_34 = Foreground256 (Just (T.ColorNumber 34))

color256_f_35 :: Foreground256
color256_f_35 = Foreground256 (Just (T.ColorNumber 35))

color256_f_36 :: Foreground256
color256_f_36 = Foreground256 (Just (T.ColorNumber 36))

color256_f_37 :: Foreground256
color256_f_37 = Foreground256 (Just (T.ColorNumber 37))

color256_f_38 :: Foreground256
color256_f_38 = Foreground256 (Just (T.ColorNumber 38))

color256_f_39 :: Foreground256
color256_f_39 = Foreground256 (Just (T.ColorNumber 39))

color256_f_40 :: Foreground256
color256_f_40 = Foreground256 (Just (T.ColorNumber 40))

color256_f_41 :: Foreground256
color256_f_41 = Foreground256 (Just (T.ColorNumber 41))

color256_f_42 :: Foreground256
color256_f_42 = Foreground256 (Just (T.ColorNumber 42))

color256_f_43 :: Foreground256
color256_f_43 = Foreground256 (Just (T.ColorNumber 43))

color256_f_44 :: Foreground256
color256_f_44 = Foreground256 (Just (T.ColorNumber 44))

color256_f_45 :: Foreground256
color256_f_45 = Foreground256 (Just (T.ColorNumber 45))

color256_f_46 :: Foreground256
color256_f_46 = Foreground256 (Just (T.ColorNumber 46))

color256_f_47 :: Foreground256
color256_f_47 = Foreground256 (Just (T.ColorNumber 47))

color256_f_48 :: Foreground256
color256_f_48 = Foreground256 (Just (T.ColorNumber 48))

color256_f_49 :: Foreground256
color256_f_49 = Foreground256 (Just (T.ColorNumber 49))

color256_f_50 :: Foreground256
color256_f_50 = Foreground256 (Just (T.ColorNumber 50))

color256_f_51 :: Foreground256
color256_f_51 = Foreground256 (Just (T.ColorNumber 51))

color256_f_52 :: Foreground256
color256_f_52 = Foreground256 (Just (T.ColorNumber 52))

color256_f_53 :: Foreground256
color256_f_53 = Foreground256 (Just (T.ColorNumber 53))

color256_f_54 :: Foreground256
color256_f_54 = Foreground256 (Just (T.ColorNumber 54))

color256_f_55 :: Foreground256
color256_f_55 = Foreground256 (Just (T.ColorNumber 55))

color256_f_56 :: Foreground256
color256_f_56 = Foreground256 (Just (T.ColorNumber 56))

color256_f_57 :: Foreground256
color256_f_57 = Foreground256 (Just (T.ColorNumber 57))

color256_f_58 :: Foreground256
color256_f_58 = Foreground256 (Just (T.ColorNumber 58))

color256_f_59 :: Foreground256
color256_f_59 = Foreground256 (Just (T.ColorNumber 59))

color256_f_60 :: Foreground256
color256_f_60 = Foreground256 (Just (T.ColorNumber 60))

color256_f_61 :: Foreground256
color256_f_61 = Foreground256 (Just (T.ColorNumber 61))

color256_f_62 :: Foreground256
color256_f_62 = Foreground256 (Just (T.ColorNumber 62))

color256_f_63 :: Foreground256
color256_f_63 = Foreground256 (Just (T.ColorNumber 63))

color256_f_64 :: Foreground256
color256_f_64 = Foreground256 (Just (T.ColorNumber 64))

color256_f_65 :: Foreground256
color256_f_65 = Foreground256 (Just (T.ColorNumber 65))

color256_f_66 :: Foreground256
color256_f_66 = Foreground256 (Just (T.ColorNumber 66))

color256_f_67 :: Foreground256
color256_f_67 = Foreground256 (Just (T.ColorNumber 67))

color256_f_68 :: Foreground256
color256_f_68 = Foreground256 (Just (T.ColorNumber 68))

color256_f_69 :: Foreground256
color256_f_69 = Foreground256 (Just (T.ColorNumber 69))

color256_f_70 :: Foreground256
color256_f_70 = Foreground256 (Just (T.ColorNumber 70))

color256_f_71 :: Foreground256
color256_f_71 = Foreground256 (Just (T.ColorNumber 71))

color256_f_72 :: Foreground256
color256_f_72 = Foreground256 (Just (T.ColorNumber 72))

color256_f_73 :: Foreground256
color256_f_73 = Foreground256 (Just (T.ColorNumber 73))

color256_f_74 :: Foreground256
color256_f_74 = Foreground256 (Just (T.ColorNumber 74))

color256_f_75 :: Foreground256
color256_f_75 = Foreground256 (Just (T.ColorNumber 75))

color256_f_76 :: Foreground256
color256_f_76 = Foreground256 (Just (T.ColorNumber 76))

color256_f_77 :: Foreground256
color256_f_77 = Foreground256 (Just (T.ColorNumber 77))

color256_f_78 :: Foreground256
color256_f_78 = Foreground256 (Just (T.ColorNumber 78))

color256_f_79 :: Foreground256
color256_f_79 = Foreground256 (Just (T.ColorNumber 79))

color256_f_80 :: Foreground256
color256_f_80 = Foreground256 (Just (T.ColorNumber 80))

color256_f_81 :: Foreground256
color256_f_81 = Foreground256 (Just (T.ColorNumber 81))

color256_f_82 :: Foreground256
color256_f_82 = Foreground256 (Just (T.ColorNumber 82))

color256_f_83 :: Foreground256
color256_f_83 = Foreground256 (Just (T.ColorNumber 83))

color256_f_84 :: Foreground256
color256_f_84 = Foreground256 (Just (T.ColorNumber 84))

color256_f_85 :: Foreground256
color256_f_85 = Foreground256 (Just (T.ColorNumber 85))

color256_f_86 :: Foreground256
color256_f_86 = Foreground256 (Just (T.ColorNumber 86))

color256_f_87 :: Foreground256
color256_f_87 = Foreground256 (Just (T.ColorNumber 87))

color256_f_88 :: Foreground256
color256_f_88 = Foreground256 (Just (T.ColorNumber 88))

color256_f_89 :: Foreground256
color256_f_89 = Foreground256 (Just (T.ColorNumber 89))

color256_f_90 :: Foreground256
color256_f_90 = Foreground256 (Just (T.ColorNumber 90))

color256_f_91 :: Foreground256
color256_f_91 = Foreground256 (Just (T.ColorNumber 91))

color256_f_92 :: Foreground256
color256_f_92 = Foreground256 (Just (T.ColorNumber 92))

color256_f_93 :: Foreground256
color256_f_93 = Foreground256 (Just (T.ColorNumber 93))

color256_f_94 :: Foreground256
color256_f_94 = Foreground256 (Just (T.ColorNumber 94))

color256_f_95 :: Foreground256
color256_f_95 = Foreground256 (Just (T.ColorNumber 95))

color256_f_96 :: Foreground256
color256_f_96 = Foreground256 (Just (T.ColorNumber 96))

color256_f_97 :: Foreground256
color256_f_97 = Foreground256 (Just (T.ColorNumber 97))

color256_f_98 :: Foreground256
color256_f_98 = Foreground256 (Just (T.ColorNumber 98))

color256_f_99 :: Foreground256
color256_f_99 = Foreground256 (Just (T.ColorNumber 99))

color256_f_100 :: Foreground256
color256_f_100 = Foreground256 (Just (T.ColorNumber 100))

color256_f_101 :: Foreground256
color256_f_101 = Foreground256 (Just (T.ColorNumber 101))

color256_f_102 :: Foreground256
color256_f_102 = Foreground256 (Just (T.ColorNumber 102))

color256_f_103 :: Foreground256
color256_f_103 = Foreground256 (Just (T.ColorNumber 103))

color256_f_104 :: Foreground256
color256_f_104 = Foreground256 (Just (T.ColorNumber 104))

color256_f_105 :: Foreground256
color256_f_105 = Foreground256 (Just (T.ColorNumber 105))

color256_f_106 :: Foreground256
color256_f_106 = Foreground256 (Just (T.ColorNumber 106))

color256_f_107 :: Foreground256
color256_f_107 = Foreground256 (Just (T.ColorNumber 107))

color256_f_108 :: Foreground256
color256_f_108 = Foreground256 (Just (T.ColorNumber 108))

color256_f_109 :: Foreground256
color256_f_109 = Foreground256 (Just (T.ColorNumber 109))

color256_f_110 :: Foreground256
color256_f_110 = Foreground256 (Just (T.ColorNumber 110))

color256_f_111 :: Foreground256
color256_f_111 = Foreground256 (Just (T.ColorNumber 111))

color256_f_112 :: Foreground256
color256_f_112 = Foreground256 (Just (T.ColorNumber 112))

color256_f_113 :: Foreground256
color256_f_113 = Foreground256 (Just (T.ColorNumber 113))

color256_f_114 :: Foreground256
color256_f_114 = Foreground256 (Just (T.ColorNumber 114))

color256_f_115 :: Foreground256
color256_f_115 = Foreground256 (Just (T.ColorNumber 115))

color256_f_116 :: Foreground256
color256_f_116 = Foreground256 (Just (T.ColorNumber 116))

color256_f_117 :: Foreground256
color256_f_117 = Foreground256 (Just (T.ColorNumber 117))

color256_f_118 :: Foreground256
color256_f_118 = Foreground256 (Just (T.ColorNumber 118))

color256_f_119 :: Foreground256
color256_f_119 = Foreground256 (Just (T.ColorNumber 119))

color256_f_120 :: Foreground256
color256_f_120 = Foreground256 (Just (T.ColorNumber 120))

color256_f_121 :: Foreground256
color256_f_121 = Foreground256 (Just (T.ColorNumber 121))

color256_f_122 :: Foreground256
color256_f_122 = Foreground256 (Just (T.ColorNumber 122))

color256_f_123 :: Foreground256
color256_f_123 = Foreground256 (Just (T.ColorNumber 123))

color256_f_124 :: Foreground256
color256_f_124 = Foreground256 (Just (T.ColorNumber 124))

color256_f_125 :: Foreground256
color256_f_125 = Foreground256 (Just (T.ColorNumber 125))

color256_f_126 :: Foreground256
color256_f_126 = Foreground256 (Just (T.ColorNumber 126))

color256_f_127 :: Foreground256
color256_f_127 = Foreground256 (Just (T.ColorNumber 127))

color256_f_128 :: Foreground256
color256_f_128 = Foreground256 (Just (T.ColorNumber 128))

color256_f_129 :: Foreground256
color256_f_129 = Foreground256 (Just (T.ColorNumber 129))

color256_f_130 :: Foreground256
color256_f_130 = Foreground256 (Just (T.ColorNumber 130))

color256_f_131 :: Foreground256
color256_f_131 = Foreground256 (Just (T.ColorNumber 131))

color256_f_132 :: Foreground256
color256_f_132 = Foreground256 (Just (T.ColorNumber 132))

color256_f_133 :: Foreground256
color256_f_133 = Foreground256 (Just (T.ColorNumber 133))

color256_f_134 :: Foreground256
color256_f_134 = Foreground256 (Just (T.ColorNumber 134))

color256_f_135 :: Foreground256
color256_f_135 = Foreground256 (Just (T.ColorNumber 135))

color256_f_136 :: Foreground256
color256_f_136 = Foreground256 (Just (T.ColorNumber 136))

color256_f_137 :: Foreground256
color256_f_137 = Foreground256 (Just (T.ColorNumber 137))

color256_f_138 :: Foreground256
color256_f_138 = Foreground256 (Just (T.ColorNumber 138))

color256_f_139 :: Foreground256
color256_f_139 = Foreground256 (Just (T.ColorNumber 139))

color256_f_140 :: Foreground256
color256_f_140 = Foreground256 (Just (T.ColorNumber 140))

color256_f_141 :: Foreground256
color256_f_141 = Foreground256 (Just (T.ColorNumber 141))

color256_f_142 :: Foreground256
color256_f_142 = Foreground256 (Just (T.ColorNumber 142))

color256_f_143 :: Foreground256
color256_f_143 = Foreground256 (Just (T.ColorNumber 143))

color256_f_144 :: Foreground256
color256_f_144 = Foreground256 (Just (T.ColorNumber 144))

color256_f_145 :: Foreground256
color256_f_145 = Foreground256 (Just (T.ColorNumber 145))

color256_f_146 :: Foreground256
color256_f_146 = Foreground256 (Just (T.ColorNumber 146))

color256_f_147 :: Foreground256
color256_f_147 = Foreground256 (Just (T.ColorNumber 147))

color256_f_148 :: Foreground256
color256_f_148 = Foreground256 (Just (T.ColorNumber 148))

color256_f_149 :: Foreground256
color256_f_149 = Foreground256 (Just (T.ColorNumber 149))

color256_f_150 :: Foreground256
color256_f_150 = Foreground256 (Just (T.ColorNumber 150))

color256_f_151 :: Foreground256
color256_f_151 = Foreground256 (Just (T.ColorNumber 151))

color256_f_152 :: Foreground256
color256_f_152 = Foreground256 (Just (T.ColorNumber 152))

color256_f_153 :: Foreground256
color256_f_153 = Foreground256 (Just (T.ColorNumber 153))

color256_f_154 :: Foreground256
color256_f_154 = Foreground256 (Just (T.ColorNumber 154))

color256_f_155 :: Foreground256
color256_f_155 = Foreground256 (Just (T.ColorNumber 155))

color256_f_156 :: Foreground256
color256_f_156 = Foreground256 (Just (T.ColorNumber 156))

color256_f_157 :: Foreground256
color256_f_157 = Foreground256 (Just (T.ColorNumber 157))

color256_f_158 :: Foreground256
color256_f_158 = Foreground256 (Just (T.ColorNumber 158))

color256_f_159 :: Foreground256
color256_f_159 = Foreground256 (Just (T.ColorNumber 159))

color256_f_160 :: Foreground256
color256_f_160 = Foreground256 (Just (T.ColorNumber 160))

color256_f_161 :: Foreground256
color256_f_161 = Foreground256 (Just (T.ColorNumber 161))

color256_f_162 :: Foreground256
color256_f_162 = Foreground256 (Just (T.ColorNumber 162))

color256_f_163 :: Foreground256
color256_f_163 = Foreground256 (Just (T.ColorNumber 163))

color256_f_164 :: Foreground256
color256_f_164 = Foreground256 (Just (T.ColorNumber 164))

color256_f_165 :: Foreground256
color256_f_165 = Foreground256 (Just (T.ColorNumber 165))

color256_f_166 :: Foreground256
color256_f_166 = Foreground256 (Just (T.ColorNumber 166))

color256_f_167 :: Foreground256
color256_f_167 = Foreground256 (Just (T.ColorNumber 167))

color256_f_168 :: Foreground256
color256_f_168 = Foreground256 (Just (T.ColorNumber 168))

color256_f_169 :: Foreground256
color256_f_169 = Foreground256 (Just (T.ColorNumber 169))

color256_f_170 :: Foreground256
color256_f_170 = Foreground256 (Just (T.ColorNumber 170))

color256_f_171 :: Foreground256
color256_f_171 = Foreground256 (Just (T.ColorNumber 171))

color256_f_172 :: Foreground256
color256_f_172 = Foreground256 (Just (T.ColorNumber 172))

color256_f_173 :: Foreground256
color256_f_173 = Foreground256 (Just (T.ColorNumber 173))

color256_f_174 :: Foreground256
color256_f_174 = Foreground256 (Just (T.ColorNumber 174))

color256_f_175 :: Foreground256
color256_f_175 = Foreground256 (Just (T.ColorNumber 175))

color256_f_176 :: Foreground256
color256_f_176 = Foreground256 (Just (T.ColorNumber 176))

color256_f_177 :: Foreground256
color256_f_177 = Foreground256 (Just (T.ColorNumber 177))

color256_f_178 :: Foreground256
color256_f_178 = Foreground256 (Just (T.ColorNumber 178))

color256_f_179 :: Foreground256
color256_f_179 = Foreground256 (Just (T.ColorNumber 179))

color256_f_180 :: Foreground256
color256_f_180 = Foreground256 (Just (T.ColorNumber 180))

color256_f_181 :: Foreground256
color256_f_181 = Foreground256 (Just (T.ColorNumber 181))

color256_f_182 :: Foreground256
color256_f_182 = Foreground256 (Just (T.ColorNumber 182))

color256_f_183 :: Foreground256
color256_f_183 = Foreground256 (Just (T.ColorNumber 183))

color256_f_184 :: Foreground256
color256_f_184 = Foreground256 (Just (T.ColorNumber 184))

color256_f_185 :: Foreground256
color256_f_185 = Foreground256 (Just (T.ColorNumber 185))

color256_f_186 :: Foreground256
color256_f_186 = Foreground256 (Just (T.ColorNumber 186))

color256_f_187 :: Foreground256
color256_f_187 = Foreground256 (Just (T.ColorNumber 187))

color256_f_188 :: Foreground256
color256_f_188 = Foreground256 (Just (T.ColorNumber 188))

color256_f_189 :: Foreground256
color256_f_189 = Foreground256 (Just (T.ColorNumber 189))

color256_f_190 :: Foreground256
color256_f_190 = Foreground256 (Just (T.ColorNumber 190))

color256_f_191 :: Foreground256
color256_f_191 = Foreground256 (Just (T.ColorNumber 191))

color256_f_192 :: Foreground256
color256_f_192 = Foreground256 (Just (T.ColorNumber 192))

color256_f_193 :: Foreground256
color256_f_193 = Foreground256 (Just (T.ColorNumber 193))

color256_f_194 :: Foreground256
color256_f_194 = Foreground256 (Just (T.ColorNumber 194))

color256_f_195 :: Foreground256
color256_f_195 = Foreground256 (Just (T.ColorNumber 195))

color256_f_196 :: Foreground256
color256_f_196 = Foreground256 (Just (T.ColorNumber 196))

color256_f_197 :: Foreground256
color256_f_197 = Foreground256 (Just (T.ColorNumber 197))

color256_f_198 :: Foreground256
color256_f_198 = Foreground256 (Just (T.ColorNumber 198))

color256_f_199 :: Foreground256
color256_f_199 = Foreground256 (Just (T.ColorNumber 199))

color256_f_200 :: Foreground256
color256_f_200 = Foreground256 (Just (T.ColorNumber 200))

color256_f_201 :: Foreground256
color256_f_201 = Foreground256 (Just (T.ColorNumber 201))

color256_f_202 :: Foreground256
color256_f_202 = Foreground256 (Just (T.ColorNumber 202))

color256_f_203 :: Foreground256
color256_f_203 = Foreground256 (Just (T.ColorNumber 203))

color256_f_204 :: Foreground256
color256_f_204 = Foreground256 (Just (T.ColorNumber 204))

color256_f_205 :: Foreground256
color256_f_205 = Foreground256 (Just (T.ColorNumber 205))

color256_f_206 :: Foreground256
color256_f_206 = Foreground256 (Just (T.ColorNumber 206))

color256_f_207 :: Foreground256
color256_f_207 = Foreground256 (Just (T.ColorNumber 207))

color256_f_208 :: Foreground256
color256_f_208 = Foreground256 (Just (T.ColorNumber 208))

color256_f_209 :: Foreground256
color256_f_209 = Foreground256 (Just (T.ColorNumber 209))

color256_f_210 :: Foreground256
color256_f_210 = Foreground256 (Just (T.ColorNumber 210))

color256_f_211 :: Foreground256
color256_f_211 = Foreground256 (Just (T.ColorNumber 211))

color256_f_212 :: Foreground256
color256_f_212 = Foreground256 (Just (T.ColorNumber 212))

color256_f_213 :: Foreground256
color256_f_213 = Foreground256 (Just (T.ColorNumber 213))

color256_f_214 :: Foreground256
color256_f_214 = Foreground256 (Just (T.ColorNumber 214))

color256_f_215 :: Foreground256
color256_f_215 = Foreground256 (Just (T.ColorNumber 215))

color256_f_216 :: Foreground256
color256_f_216 = Foreground256 (Just (T.ColorNumber 216))

color256_f_217 :: Foreground256
color256_f_217 = Foreground256 (Just (T.ColorNumber 217))

color256_f_218 :: Foreground256
color256_f_218 = Foreground256 (Just (T.ColorNumber 218))

color256_f_219 :: Foreground256
color256_f_219 = Foreground256 (Just (T.ColorNumber 219))

color256_f_220 :: Foreground256
color256_f_220 = Foreground256 (Just (T.ColorNumber 220))

color256_f_221 :: Foreground256
color256_f_221 = Foreground256 (Just (T.ColorNumber 221))

color256_f_222 :: Foreground256
color256_f_222 = Foreground256 (Just (T.ColorNumber 222))

color256_f_223 :: Foreground256
color256_f_223 = Foreground256 (Just (T.ColorNumber 223))

color256_f_224 :: Foreground256
color256_f_224 = Foreground256 (Just (T.ColorNumber 224))

color256_f_225 :: Foreground256
color256_f_225 = Foreground256 (Just (T.ColorNumber 225))

color256_f_226 :: Foreground256
color256_f_226 = Foreground256 (Just (T.ColorNumber 226))

color256_f_227 :: Foreground256
color256_f_227 = Foreground256 (Just (T.ColorNumber 227))

color256_f_228 :: Foreground256
color256_f_228 = Foreground256 (Just (T.ColorNumber 228))

color256_f_229 :: Foreground256
color256_f_229 = Foreground256 (Just (T.ColorNumber 229))

color256_f_230 :: Foreground256
color256_f_230 = Foreground256 (Just (T.ColorNumber 230))

color256_f_231 :: Foreground256
color256_f_231 = Foreground256 (Just (T.ColorNumber 231))

color256_f_232 :: Foreground256
color256_f_232 = Foreground256 (Just (T.ColorNumber 232))

color256_f_233 :: Foreground256
color256_f_233 = Foreground256 (Just (T.ColorNumber 233))

color256_f_234 :: Foreground256
color256_f_234 = Foreground256 (Just (T.ColorNumber 234))

color256_f_235 :: Foreground256
color256_f_235 = Foreground256 (Just (T.ColorNumber 235))

color256_f_236 :: Foreground256
color256_f_236 = Foreground256 (Just (T.ColorNumber 236))

color256_f_237 :: Foreground256
color256_f_237 = Foreground256 (Just (T.ColorNumber 237))

color256_f_238 :: Foreground256
color256_f_238 = Foreground256 (Just (T.ColorNumber 238))

color256_f_239 :: Foreground256
color256_f_239 = Foreground256 (Just (T.ColorNumber 239))

color256_f_240 :: Foreground256
color256_f_240 = Foreground256 (Just (T.ColorNumber 240))

color256_f_241 :: Foreground256
color256_f_241 = Foreground256 (Just (T.ColorNumber 241))

color256_f_242 :: Foreground256
color256_f_242 = Foreground256 (Just (T.ColorNumber 242))

color256_f_243 :: Foreground256
color256_f_243 = Foreground256 (Just (T.ColorNumber 243))

color256_f_244 :: Foreground256
color256_f_244 = Foreground256 (Just (T.ColorNumber 244))

color256_f_245 :: Foreground256
color256_f_245 = Foreground256 (Just (T.ColorNumber 245))

color256_f_246 :: Foreground256
color256_f_246 = Foreground256 (Just (T.ColorNumber 246))

color256_f_247 :: Foreground256
color256_f_247 = Foreground256 (Just (T.ColorNumber 247))

color256_f_248 :: Foreground256
color256_f_248 = Foreground256 (Just (T.ColorNumber 248))

color256_f_249 :: Foreground256
color256_f_249 = Foreground256 (Just (T.ColorNumber 249))

color256_f_250 :: Foreground256
color256_f_250 = Foreground256 (Just (T.ColorNumber 250))

color256_f_251 :: Foreground256
color256_f_251 = Foreground256 (Just (T.ColorNumber 251))

color256_f_252 :: Foreground256
color256_f_252 = Foreground256 (Just (T.ColorNumber 252))

color256_f_253 :: Foreground256
color256_f_253 = Foreground256 (Just (T.ColorNumber 253))

color256_f_254 :: Foreground256
color256_f_254 = Foreground256 (Just (T.ColorNumber 254))

color256_f_255 :: Foreground256
color256_f_255 = Foreground256 (Just (T.ColorNumber 255))

color256_b_default :: Background256
color256_b_default = Background256 Nothing

color256_b_0 :: Background256
color256_b_0 = Background256 (Just (T.ColorNumber 0))

color256_b_black :: Background256
color256_b_black = Background256 (Just (T.ColorNumber 0))

color256_b_1 :: Background256
color256_b_1 = Background256 (Just (T.ColorNumber 1))

color256_b_red :: Background256
color256_b_red = Background256 (Just (T.ColorNumber 1))

color256_b_2 :: Background256
color256_b_2 = Background256 (Just (T.ColorNumber 2))

color256_b_green :: Background256
color256_b_green = Background256 (Just (T.ColorNumber 2))

color256_b_3 :: Background256
color256_b_3 = Background256 (Just (T.ColorNumber 3))

color256_b_yellow :: Background256
color256_b_yellow = Background256 (Just (T.ColorNumber 3))

color256_b_4 :: Background256
color256_b_4 = Background256 (Just (T.ColorNumber 4))

color256_b_blue :: Background256
color256_b_blue = Background256 (Just (T.ColorNumber 4))

color256_b_5 :: Background256
color256_b_5 = Background256 (Just (T.ColorNumber 5))

color256_b_magenta :: Background256
color256_b_magenta = Background256 (Just (T.ColorNumber 5))

color256_b_6 :: Background256
color256_b_6 = Background256 (Just (T.ColorNumber 6))

color256_b_cyan :: Background256
color256_b_cyan = Background256 (Just (T.ColorNumber 6))

color256_b_7 :: Background256
color256_b_7 = Background256 (Just (T.ColorNumber 7))

color256_b_white :: Background256
color256_b_white = Background256 (Just (T.ColorNumber 7))

color256_b_8 :: Background256
color256_b_8 = Background256 (Just (T.ColorNumber 8))

color256_b_grey :: Background256
color256_b_grey = Background256 (Just (T.ColorNumber 8))

color256_b_9 :: Background256
color256_b_9 = Background256 (Just (T.ColorNumber 9))

color256_b_red_bright :: Background256
color256_b_red_bright = Background256 (Just (T.ColorNumber 9))

color256_b_10 :: Background256
color256_b_10 = Background256 (Just (T.ColorNumber 10))

color256_b_green_bright :: Background256
color256_b_green_bright = Background256 (Just (T.ColorNumber 10))

color256_b_11 :: Background256
color256_b_11 = Background256 (Just (T.ColorNumber 11))

color256_b_yellow_bright :: Background256
color256_b_yellow_bright = Background256 (Just (T.ColorNumber 11))

color256_b_12 :: Background256
color256_b_12 = Background256 (Just (T.ColorNumber 12))

color256_b_blue_bright :: Background256
color256_b_blue_bright = Background256 (Just (T.ColorNumber 12))

color256_b_13 :: Background256
color256_b_13 = Background256 (Just (T.ColorNumber 13))

color256_b_magenta_bright :: Background256
color256_b_magenta_bright = Background256 (Just (T.ColorNumber 13))

color256_b_14 :: Background256
color256_b_14 = Background256 (Just (T.ColorNumber 14))

color256_b_cyan_bright :: Background256
color256_b_cyan_bright = Background256 (Just (T.ColorNumber 14))

color256_b_15 :: Background256
color256_b_15 = Background256 (Just (T.ColorNumber 15))

color256_b_white_bright :: Background256
color256_b_white_bright = Background256 (Just (T.ColorNumber 15))

color256_b_16 :: Background256
color256_b_16 = Background256 (Just (T.ColorNumber 16))

color256_b_17 :: Background256
color256_b_17 = Background256 (Just (T.ColorNumber 17))

color256_b_18 :: Background256
color256_b_18 = Background256 (Just (T.ColorNumber 18))

color256_b_19 :: Background256
color256_b_19 = Background256 (Just (T.ColorNumber 19))

color256_b_20 :: Background256
color256_b_20 = Background256 (Just (T.ColorNumber 20))

color256_b_21 :: Background256
color256_b_21 = Background256 (Just (T.ColorNumber 21))

color256_b_22 :: Background256
color256_b_22 = Background256 (Just (T.ColorNumber 22))

color256_b_23 :: Background256
color256_b_23 = Background256 (Just (T.ColorNumber 23))

color256_b_24 :: Background256
color256_b_24 = Background256 (Just (T.ColorNumber 24))

color256_b_25 :: Background256
color256_b_25 = Background256 (Just (T.ColorNumber 25))

color256_b_26 :: Background256
color256_b_26 = Background256 (Just (T.ColorNumber 26))

color256_b_27 :: Background256
color256_b_27 = Background256 (Just (T.ColorNumber 27))

color256_b_28 :: Background256
color256_b_28 = Background256 (Just (T.ColorNumber 28))

color256_b_29 :: Background256
color256_b_29 = Background256 (Just (T.ColorNumber 29))

color256_b_30 :: Background256
color256_b_30 = Background256 (Just (T.ColorNumber 30))

color256_b_31 :: Background256
color256_b_31 = Background256 (Just (T.ColorNumber 31))

color256_b_32 :: Background256
color256_b_32 = Background256 (Just (T.ColorNumber 32))

color256_b_33 :: Background256
color256_b_33 = Background256 (Just (T.ColorNumber 33))

color256_b_34 :: Background256
color256_b_34 = Background256 (Just (T.ColorNumber 34))

color256_b_35 :: Background256
color256_b_35 = Background256 (Just (T.ColorNumber 35))

color256_b_36 :: Background256
color256_b_36 = Background256 (Just (T.ColorNumber 36))

color256_b_37 :: Background256
color256_b_37 = Background256 (Just (T.ColorNumber 37))

color256_b_38 :: Background256
color256_b_38 = Background256 (Just (T.ColorNumber 38))

color256_b_39 :: Background256
color256_b_39 = Background256 (Just (T.ColorNumber 39))

color256_b_40 :: Background256
color256_b_40 = Background256 (Just (T.ColorNumber 40))

color256_b_41 :: Background256
color256_b_41 = Background256 (Just (T.ColorNumber 41))

color256_b_42 :: Background256
color256_b_42 = Background256 (Just (T.ColorNumber 42))

color256_b_43 :: Background256
color256_b_43 = Background256 (Just (T.ColorNumber 43))

color256_b_44 :: Background256
color256_b_44 = Background256 (Just (T.ColorNumber 44))

color256_b_45 :: Background256
color256_b_45 = Background256 (Just (T.ColorNumber 45))

color256_b_46 :: Background256
color256_b_46 = Background256 (Just (T.ColorNumber 46))

color256_b_47 :: Background256
color256_b_47 = Background256 (Just (T.ColorNumber 47))

color256_b_48 :: Background256
color256_b_48 = Background256 (Just (T.ColorNumber 48))

color256_b_49 :: Background256
color256_b_49 = Background256 (Just (T.ColorNumber 49))

color256_b_50 :: Background256
color256_b_50 = Background256 (Just (T.ColorNumber 50))

color256_b_51 :: Background256
color256_b_51 = Background256 (Just (T.ColorNumber 51))

color256_b_52 :: Background256
color256_b_52 = Background256 (Just (T.ColorNumber 52))

color256_b_53 :: Background256
color256_b_53 = Background256 (Just (T.ColorNumber 53))

color256_b_54 :: Background256
color256_b_54 = Background256 (Just (T.ColorNumber 54))

color256_b_55 :: Background256
color256_b_55 = Background256 (Just (T.ColorNumber 55))

color256_b_56 :: Background256
color256_b_56 = Background256 (Just (T.ColorNumber 56))

color256_b_57 :: Background256
color256_b_57 = Background256 (Just (T.ColorNumber 57))

color256_b_58 :: Background256
color256_b_58 = Background256 (Just (T.ColorNumber 58))

color256_b_59 :: Background256
color256_b_59 = Background256 (Just (T.ColorNumber 59))

color256_b_60 :: Background256
color256_b_60 = Background256 (Just (T.ColorNumber 60))

color256_b_61 :: Background256
color256_b_61 = Background256 (Just (T.ColorNumber 61))

color256_b_62 :: Background256
color256_b_62 = Background256 (Just (T.ColorNumber 62))

color256_b_63 :: Background256
color256_b_63 = Background256 (Just (T.ColorNumber 63))

color256_b_64 :: Background256
color256_b_64 = Background256 (Just (T.ColorNumber 64))

color256_b_65 :: Background256
color256_b_65 = Background256 (Just (T.ColorNumber 65))

color256_b_66 :: Background256
color256_b_66 = Background256 (Just (T.ColorNumber 66))

color256_b_67 :: Background256
color256_b_67 = Background256 (Just (T.ColorNumber 67))

color256_b_68 :: Background256
color256_b_68 = Background256 (Just (T.ColorNumber 68))

color256_b_69 :: Background256
color256_b_69 = Background256 (Just (T.ColorNumber 69))

color256_b_70 :: Background256
color256_b_70 = Background256 (Just (T.ColorNumber 70))

color256_b_71 :: Background256
color256_b_71 = Background256 (Just (T.ColorNumber 71))

color256_b_72 :: Background256
color256_b_72 = Background256 (Just (T.ColorNumber 72))

color256_b_73 :: Background256
color256_b_73 = Background256 (Just (T.ColorNumber 73))

color256_b_74 :: Background256
color256_b_74 = Background256 (Just (T.ColorNumber 74))

color256_b_75 :: Background256
color256_b_75 = Background256 (Just (T.ColorNumber 75))

color256_b_76 :: Background256
color256_b_76 = Background256 (Just (T.ColorNumber 76))

color256_b_77 :: Background256
color256_b_77 = Background256 (Just (T.ColorNumber 77))

color256_b_78 :: Background256
color256_b_78 = Background256 (Just (T.ColorNumber 78))

color256_b_79 :: Background256
color256_b_79 = Background256 (Just (T.ColorNumber 79))

color256_b_80 :: Background256
color256_b_80 = Background256 (Just (T.ColorNumber 80))

color256_b_81 :: Background256
color256_b_81 = Background256 (Just (T.ColorNumber 81))

color256_b_82 :: Background256
color256_b_82 = Background256 (Just (T.ColorNumber 82))

color256_b_83 :: Background256
color256_b_83 = Background256 (Just (T.ColorNumber 83))

color256_b_84 :: Background256
color256_b_84 = Background256 (Just (T.ColorNumber 84))

color256_b_85 :: Background256
color256_b_85 = Background256 (Just (T.ColorNumber 85))

color256_b_86 :: Background256
color256_b_86 = Background256 (Just (T.ColorNumber 86))

color256_b_87 :: Background256
color256_b_87 = Background256 (Just (T.ColorNumber 87))

color256_b_88 :: Background256
color256_b_88 = Background256 (Just (T.ColorNumber 88))

color256_b_89 :: Background256
color256_b_89 = Background256 (Just (T.ColorNumber 89))

color256_b_90 :: Background256
color256_b_90 = Background256 (Just (T.ColorNumber 90))

color256_b_91 :: Background256
color256_b_91 = Background256 (Just (T.ColorNumber 91))

color256_b_92 :: Background256
color256_b_92 = Background256 (Just (T.ColorNumber 92))

color256_b_93 :: Background256
color256_b_93 = Background256 (Just (T.ColorNumber 93))

color256_b_94 :: Background256
color256_b_94 = Background256 (Just (T.ColorNumber 94))

color256_b_95 :: Background256
color256_b_95 = Background256 (Just (T.ColorNumber 95))

color256_b_96 :: Background256
color256_b_96 = Background256 (Just (T.ColorNumber 96))

color256_b_97 :: Background256
color256_b_97 = Background256 (Just (T.ColorNumber 97))

color256_b_98 :: Background256
color256_b_98 = Background256 (Just (T.ColorNumber 98))

color256_b_99 :: Background256
color256_b_99 = Background256 (Just (T.ColorNumber 99))

color256_b_100 :: Background256
color256_b_100 = Background256 (Just (T.ColorNumber 100))

color256_b_101 :: Background256
color256_b_101 = Background256 (Just (T.ColorNumber 101))

color256_b_102 :: Background256
color256_b_102 = Background256 (Just (T.ColorNumber 102))

color256_b_103 :: Background256
color256_b_103 = Background256 (Just (T.ColorNumber 103))

color256_b_104 :: Background256
color256_b_104 = Background256 (Just (T.ColorNumber 104))

color256_b_105 :: Background256
color256_b_105 = Background256 (Just (T.ColorNumber 105))

color256_b_106 :: Background256
color256_b_106 = Background256 (Just (T.ColorNumber 106))

color256_b_107 :: Background256
color256_b_107 = Background256 (Just (T.ColorNumber 107))

color256_b_108 :: Background256
color256_b_108 = Background256 (Just (T.ColorNumber 108))

color256_b_109 :: Background256
color256_b_109 = Background256 (Just (T.ColorNumber 109))

color256_b_110 :: Background256
color256_b_110 = Background256 (Just (T.ColorNumber 110))

color256_b_111 :: Background256
color256_b_111 = Background256 (Just (T.ColorNumber 111))

color256_b_112 :: Background256
color256_b_112 = Background256 (Just (T.ColorNumber 112))

color256_b_113 :: Background256
color256_b_113 = Background256 (Just (T.ColorNumber 113))

color256_b_114 :: Background256
color256_b_114 = Background256 (Just (T.ColorNumber 114))

color256_b_115 :: Background256
color256_b_115 = Background256 (Just (T.ColorNumber 115))

color256_b_116 :: Background256
color256_b_116 = Background256 (Just (T.ColorNumber 116))

color256_b_117 :: Background256
color256_b_117 = Background256 (Just (T.ColorNumber 117))

color256_b_118 :: Background256
color256_b_118 = Background256 (Just (T.ColorNumber 118))

color256_b_119 :: Background256
color256_b_119 = Background256 (Just (T.ColorNumber 119))

color256_b_120 :: Background256
color256_b_120 = Background256 (Just (T.ColorNumber 120))

color256_b_121 :: Background256
color256_b_121 = Background256 (Just (T.ColorNumber 121))

color256_b_122 :: Background256
color256_b_122 = Background256 (Just (T.ColorNumber 122))

color256_b_123 :: Background256
color256_b_123 = Background256 (Just (T.ColorNumber 123))

color256_b_124 :: Background256
color256_b_124 = Background256 (Just (T.ColorNumber 124))

color256_b_125 :: Background256
color256_b_125 = Background256 (Just (T.ColorNumber 125))

color256_b_126 :: Background256
color256_b_126 = Background256 (Just (T.ColorNumber 126))

color256_b_127 :: Background256
color256_b_127 = Background256 (Just (T.ColorNumber 127))

color256_b_128 :: Background256
color256_b_128 = Background256 (Just (T.ColorNumber 128))

color256_b_129 :: Background256
color256_b_129 = Background256 (Just (T.ColorNumber 129))

color256_b_130 :: Background256
color256_b_130 = Background256 (Just (T.ColorNumber 130))

color256_b_131 :: Background256
color256_b_131 = Background256 (Just (T.ColorNumber 131))

color256_b_132 :: Background256
color256_b_132 = Background256 (Just (T.ColorNumber 132))

color256_b_133 :: Background256
color256_b_133 = Background256 (Just (T.ColorNumber 133))

color256_b_134 :: Background256
color256_b_134 = Background256 (Just (T.ColorNumber 134))

color256_b_135 :: Background256
color256_b_135 = Background256 (Just (T.ColorNumber 135))

color256_b_136 :: Background256
color256_b_136 = Background256 (Just (T.ColorNumber 136))

color256_b_137 :: Background256
color256_b_137 = Background256 (Just (T.ColorNumber 137))

color256_b_138 :: Background256
color256_b_138 = Background256 (Just (T.ColorNumber 138))

color256_b_139 :: Background256
color256_b_139 = Background256 (Just (T.ColorNumber 139))

color256_b_140 :: Background256
color256_b_140 = Background256 (Just (T.ColorNumber 140))

color256_b_141 :: Background256
color256_b_141 = Background256 (Just (T.ColorNumber 141))

color256_b_142 :: Background256
color256_b_142 = Background256 (Just (T.ColorNumber 142))

color256_b_143 :: Background256
color256_b_143 = Background256 (Just (T.ColorNumber 143))

color256_b_144 :: Background256
color256_b_144 = Background256 (Just (T.ColorNumber 144))

color256_b_145 :: Background256
color256_b_145 = Background256 (Just (T.ColorNumber 145))

color256_b_146 :: Background256
color256_b_146 = Background256 (Just (T.ColorNumber 146))

color256_b_147 :: Background256
color256_b_147 = Background256 (Just (T.ColorNumber 147))

color256_b_148 :: Background256
color256_b_148 = Background256 (Just (T.ColorNumber 148))

color256_b_149 :: Background256
color256_b_149 = Background256 (Just (T.ColorNumber 149))

color256_b_150 :: Background256
color256_b_150 = Background256 (Just (T.ColorNumber 150))

color256_b_151 :: Background256
color256_b_151 = Background256 (Just (T.ColorNumber 151))

color256_b_152 :: Background256
color256_b_152 = Background256 (Just (T.ColorNumber 152))

color256_b_153 :: Background256
color256_b_153 = Background256 (Just (T.ColorNumber 153))

color256_b_154 :: Background256
color256_b_154 = Background256 (Just (T.ColorNumber 154))

color256_b_155 :: Background256
color256_b_155 = Background256 (Just (T.ColorNumber 155))

color256_b_156 :: Background256
color256_b_156 = Background256 (Just (T.ColorNumber 156))

color256_b_157 :: Background256
color256_b_157 = Background256 (Just (T.ColorNumber 157))

color256_b_158 :: Background256
color256_b_158 = Background256 (Just (T.ColorNumber 158))

color256_b_159 :: Background256
color256_b_159 = Background256 (Just (T.ColorNumber 159))

color256_b_160 :: Background256
color256_b_160 = Background256 (Just (T.ColorNumber 160))

color256_b_161 :: Background256
color256_b_161 = Background256 (Just (T.ColorNumber 161))

color256_b_162 :: Background256
color256_b_162 = Background256 (Just (T.ColorNumber 162))

color256_b_163 :: Background256
color256_b_163 = Background256 (Just (T.ColorNumber 163))

color256_b_164 :: Background256
color256_b_164 = Background256 (Just (T.ColorNumber 164))

color256_b_165 :: Background256
color256_b_165 = Background256 (Just (T.ColorNumber 165))

color256_b_166 :: Background256
color256_b_166 = Background256 (Just (T.ColorNumber 166))

color256_b_167 :: Background256
color256_b_167 = Background256 (Just (T.ColorNumber 167))

color256_b_168 :: Background256
color256_b_168 = Background256 (Just (T.ColorNumber 168))

color256_b_169 :: Background256
color256_b_169 = Background256 (Just (T.ColorNumber 169))

color256_b_170 :: Background256
color256_b_170 = Background256 (Just (T.ColorNumber 170))

color256_b_171 :: Background256
color256_b_171 = Background256 (Just (T.ColorNumber 171))

color256_b_172 :: Background256
color256_b_172 = Background256 (Just (T.ColorNumber 172))

color256_b_173 :: Background256
color256_b_173 = Background256 (Just (T.ColorNumber 173))

color256_b_174 :: Background256
color256_b_174 = Background256 (Just (T.ColorNumber 174))

color256_b_175 :: Background256
color256_b_175 = Background256 (Just (T.ColorNumber 175))

color256_b_176 :: Background256
color256_b_176 = Background256 (Just (T.ColorNumber 176))

color256_b_177 :: Background256
color256_b_177 = Background256 (Just (T.ColorNumber 177))

color256_b_178 :: Background256
color256_b_178 = Background256 (Just (T.ColorNumber 178))

color256_b_179 :: Background256
color256_b_179 = Background256 (Just (T.ColorNumber 179))

color256_b_180 :: Background256
color256_b_180 = Background256 (Just (T.ColorNumber 180))

color256_b_181 :: Background256
color256_b_181 = Background256 (Just (T.ColorNumber 181))

color256_b_182 :: Background256
color256_b_182 = Background256 (Just (T.ColorNumber 182))

color256_b_183 :: Background256
color256_b_183 = Background256 (Just (T.ColorNumber 183))

color256_b_184 :: Background256
color256_b_184 = Background256 (Just (T.ColorNumber 184))

color256_b_185 :: Background256
color256_b_185 = Background256 (Just (T.ColorNumber 185))

color256_b_186 :: Background256
color256_b_186 = Background256 (Just (T.ColorNumber 186))

color256_b_187 :: Background256
color256_b_187 = Background256 (Just (T.ColorNumber 187))

color256_b_188 :: Background256
color256_b_188 = Background256 (Just (T.ColorNumber 188))

color256_b_189 :: Background256
color256_b_189 = Background256 (Just (T.ColorNumber 189))

color256_b_190 :: Background256
color256_b_190 = Background256 (Just (T.ColorNumber 190))

color256_b_191 :: Background256
color256_b_191 = Background256 (Just (T.ColorNumber 191))

color256_b_192 :: Background256
color256_b_192 = Background256 (Just (T.ColorNumber 192))

color256_b_193 :: Background256
color256_b_193 = Background256 (Just (T.ColorNumber 193))

color256_b_194 :: Background256
color256_b_194 = Background256 (Just (T.ColorNumber 194))

color256_b_195 :: Background256
color256_b_195 = Background256 (Just (T.ColorNumber 195))

color256_b_196 :: Background256
color256_b_196 = Background256 (Just (T.ColorNumber 196))

color256_b_197 :: Background256
color256_b_197 = Background256 (Just (T.ColorNumber 197))

color256_b_198 :: Background256
color256_b_198 = Background256 (Just (T.ColorNumber 198))

color256_b_199 :: Background256
color256_b_199 = Background256 (Just (T.ColorNumber 199))

color256_b_200 :: Background256
color256_b_200 = Background256 (Just (T.ColorNumber 200))

color256_b_201 :: Background256
color256_b_201 = Background256 (Just (T.ColorNumber 201))

color256_b_202 :: Background256
color256_b_202 = Background256 (Just (T.ColorNumber 202))

color256_b_203 :: Background256
color256_b_203 = Background256 (Just (T.ColorNumber 203))

color256_b_204 :: Background256
color256_b_204 = Background256 (Just (T.ColorNumber 204))

color256_b_205 :: Background256
color256_b_205 = Background256 (Just (T.ColorNumber 205))

color256_b_206 :: Background256
color256_b_206 = Background256 (Just (T.ColorNumber 206))

color256_b_207 :: Background256
color256_b_207 = Background256 (Just (T.ColorNumber 207))

color256_b_208 :: Background256
color256_b_208 = Background256 (Just (T.ColorNumber 208))

color256_b_209 :: Background256
color256_b_209 = Background256 (Just (T.ColorNumber 209))

color256_b_210 :: Background256
color256_b_210 = Background256 (Just (T.ColorNumber 210))

color256_b_211 :: Background256
color256_b_211 = Background256 (Just (T.ColorNumber 211))

color256_b_212 :: Background256
color256_b_212 = Background256 (Just (T.ColorNumber 212))

color256_b_213 :: Background256
color256_b_213 = Background256 (Just (T.ColorNumber 213))

color256_b_214 :: Background256
color256_b_214 = Background256 (Just (T.ColorNumber 214))

color256_b_215 :: Background256
color256_b_215 = Background256 (Just (T.ColorNumber 215))

color256_b_216 :: Background256
color256_b_216 = Background256 (Just (T.ColorNumber 216))

color256_b_217 :: Background256
color256_b_217 = Background256 (Just (T.ColorNumber 217))

color256_b_218 :: Background256
color256_b_218 = Background256 (Just (T.ColorNumber 218))

color256_b_219 :: Background256
color256_b_219 = Background256 (Just (T.ColorNumber 219))

color256_b_220 :: Background256
color256_b_220 = Background256 (Just (T.ColorNumber 220))

color256_b_221 :: Background256
color256_b_221 = Background256 (Just (T.ColorNumber 221))

color256_b_222 :: Background256
color256_b_222 = Background256 (Just (T.ColorNumber 222))

color256_b_223 :: Background256
color256_b_223 = Background256 (Just (T.ColorNumber 223))

color256_b_224 :: Background256
color256_b_224 = Background256 (Just (T.ColorNumber 224))

color256_b_225 :: Background256
color256_b_225 = Background256 (Just (T.ColorNumber 225))

color256_b_226 :: Background256
color256_b_226 = Background256 (Just (T.ColorNumber 226))

color256_b_227 :: Background256
color256_b_227 = Background256 (Just (T.ColorNumber 227))

color256_b_228 :: Background256
color256_b_228 = Background256 (Just (T.ColorNumber 228))

color256_b_229 :: Background256
color256_b_229 = Background256 (Just (T.ColorNumber 229))

color256_b_230 :: Background256
color256_b_230 = Background256 (Just (T.ColorNumber 230))

color256_b_231 :: Background256
color256_b_231 = Background256 (Just (T.ColorNumber 231))

color256_b_232 :: Background256
color256_b_232 = Background256 (Just (T.ColorNumber 232))

color256_b_233 :: Background256
color256_b_233 = Background256 (Just (T.ColorNumber 233))

color256_b_234 :: Background256
color256_b_234 = Background256 (Just (T.ColorNumber 234))

color256_b_235 :: Background256
color256_b_235 = Background256 (Just (T.ColorNumber 235))

color256_b_236 :: Background256
color256_b_236 = Background256 (Just (T.ColorNumber 236))

color256_b_237 :: Background256
color256_b_237 = Background256 (Just (T.ColorNumber 237))

color256_b_238 :: Background256
color256_b_238 = Background256 (Just (T.ColorNumber 238))

color256_b_239 :: Background256
color256_b_239 = Background256 (Just (T.ColorNumber 239))

color256_b_240 :: Background256
color256_b_240 = Background256 (Just (T.ColorNumber 240))

color256_b_241 :: Background256
color256_b_241 = Background256 (Just (T.ColorNumber 241))

color256_b_242 :: Background256
color256_b_242 = Background256 (Just (T.ColorNumber 242))

color256_b_243 :: Background256
color256_b_243 = Background256 (Just (T.ColorNumber 243))

color256_b_244 :: Background256
color256_b_244 = Background256 (Just (T.ColorNumber 244))

color256_b_245 :: Background256
color256_b_245 = Background256 (Just (T.ColorNumber 245))

color256_b_246 :: Background256
color256_b_246 = Background256 (Just (T.ColorNumber 246))

color256_b_247 :: Background256
color256_b_247 = Background256 (Just (T.ColorNumber 247))

color256_b_248 :: Background256
color256_b_248 = Background256 (Just (T.ColorNumber 248))

color256_b_249 :: Background256
color256_b_249 = Background256 (Just (T.ColorNumber 249))

color256_b_250 :: Background256
color256_b_250 = Background256 (Just (T.ColorNumber 250))

color256_b_251 :: Background256
color256_b_251 = Background256 (Just (T.ColorNumber 251))

color256_b_252 :: Background256
color256_b_252 = Background256 (Just (T.ColorNumber 252))

color256_b_253 :: Background256
color256_b_253 = Background256 (Just (T.ColorNumber 253))

color256_b_254 :: Background256
color256_b_254 = Background256 (Just (T.ColorNumber 254))

color256_b_255 :: Background256
color256_b_255 = Background256 (Just (T.ColorNumber 255))

