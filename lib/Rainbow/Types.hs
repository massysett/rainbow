{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

-- | The innards of Rainbow.  Ordinarily you should not need this
-- module; instead, just import "Rainbow", which
-- re-exports the most useful names from this module.

module Rainbow.Types where

-- # Imports

import qualified Data.String as Str
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as X
import qualified Data.Text.Lazy as XL
import Data.Word (Word8)
import GHC.Generics
import Data.Typeable

-- For Background8, Background256, Foreground8, Foreground256: the
-- Last wraps a Maybe (Terminfo Color). If the inner Maybe is Nothing,
-- use the default color.

type Background8 = Last Color8
type Background256 = Last Color256
type Foreground8 = Last Color8
type Foreground256 = Last Color256

--
-- Colors
--

-- | A simple enumeration for eight values.
data Enum8
  = E0
  | E1
  | E2
  | E3
  | E4
  | E5
  | E6
  | E7
  deriving (Eq, Ord, Show, Bounded, Enum, Generic, Typeable)

enum8toWord8 :: Enum8 -> Word8
enum8toWord8 e = case e of
  E0 -> 0
  E1 -> 1
  E2 -> 2
  E3 -> 3
  E4 -> 4
  E5 -> 5
  E6 -> 6
  E7 -> 7

-- | Color for an 8-color terminal.  Does not affect 256-color
-- terminals.

newtype Color8 = Color8
  { unColor8 :: Maybe Enum8
  -- ^ Nothing indicates to use the default color for the terminal;
  -- otherwise, use the corresponding Terminfo 'T.Color'.
  } deriving (Eq, Ord, Show, Generic, Typeable)

-- | Color for an 256-color terminal.  Does not affect 8-color
-- terminals.

newtype Color256 = Color256
  { unColor256 :: Maybe Word8
  -- ^ Nothing indicates to use the default color for the terminal;
  -- otherwise, use the corresponding Terminfo 'T.Color'.
  } deriving (Eq, Ord, Show, Generic, Typeable)

-- | Any color for an 8-color terminal can also be used in a
-- 256-color terminal.
to256 :: Color8 -> Color256
to256 (Color8 mayE) = Color256 $ fmap enum8toWord8 mayE

--
-- Styles
--

-- | Style elements that apply in both 8 and 256 color
-- terminals. However, the elements are described separately for 8 and
-- 256 color terminals, so that the text appearance can change
-- depending on how many colors a terminal has.
data StyleCommon = StyleCommon
  { scBold :: Last Bool
  , scFaint :: Last Bool
  , scItalic :: Last Bool
  , scUnderline :: Last Bool
  , scBlink :: Last Bool
  , scInverse :: Last Bool
  , scInvisible :: Last Bool
  , scStrikeout :: Last Bool
  } deriving (Show, Eq, Ord, Generic, Typeable)


instance Monoid StyleCommon where
  mempty = StyleCommon (Last Nothing) (Last Nothing)
                       (Last Nothing) (Last Nothing)
                       (Last Nothing) (Last Nothing)
                       (Last Nothing) (Last Nothing)
  mappend (StyleCommon x1 x2 x3 x4 x5 x6 x7 x8)
          (StyleCommon y1 y2 y3 y4 y5 y6 y7 y8)
    = StyleCommon (x1 <> y1) (x2 <> y2) (x3 <> y3) (x4 <> y4)
                  (x5 <> y5) (x6 <> y6) (x7 <> y7) (x8 <> y8)

-- | Describes text appearance (foreground and background colors, as
-- well as other attributes such as bold) for an 8 color terminal.
data Style8 = Style8
  { foreground8 :: Foreground8
  , background8 :: Background8
  , common8 :: StyleCommon
  } deriving (Show, Eq, Ord, Generic, Typeable)


instance Monoid Style8 where
  mappend (Style8 fx bx cx) (Style8 fy by cy)
    = Style8 (fx <> fy) (bx <> by) (cx <> cy)
  mempty = Style8 mempty mempty mempty

-- | Describes text appearance (foreground and background colors, as
-- well as other attributes such as bold) for a 256 color terminal.
data Style256 = Style256
  { foreground256 :: Foreground256
  , background256 :: Background256
  , common256 :: StyleCommon
  } deriving (Show, Eq, Ord, Generic, Typeable)


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
  { style8 :: Style8
  , style256 :: Style256
  } deriving (Show, Eq, Ord, Generic, Typeable)


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
-- a 256 color terminal.

data Chunk = Chunk
  { chunkTextSpec :: TextSpec
    -- ^ Specifies all the effects (such as bold, underlining,
    -- colors, etc) that apply to this chunk, with different effects for
    -- 8 and 256 color terminals; and
    --
  , chunkTexts :: [Text]
    -- The text that is in this chunk.  When printing the
    -- 'Chunk', first all colors and effects on the terminal are reset.
    -- Then, the effects in the 'TextSpec' are applied, and then the
    -- 'Text's are printed by encoding them to UTF-8.  Then, all the
    -- colors and effects on the terminal are again reset.
    --
    -- Each text in this list is a strict 'X.Text'; though there is no
    -- provision for lazy 'X.Text', you can get the same effect as a lazy
    -- 'X.Text' by using a list of strict 'X.Text'.  'chunkFromLazyText'
    -- 'Text' by using a list of strict 'and 'chunkFromLazyTexts' do
    -- 'Text' by using a list of strict 'this for you.
  } deriving (Eq, Show, Ord, Generic, Typeable)
  

instance Str.IsString Chunk where
  fromString s = Chunk mempty [(X.pack s)]

-- | Creates a 'Chunk' from a strict 'X.Text' with default colors
-- and no special effects.
chunkFromText :: X.Text -> Chunk
chunkFromText = Chunk mempty . (:[])

-- | Creates a 'Chunk' from a list of strict 'X.Text' with default
-- colors and no special effects.
chunkFromTexts :: [X.Text] -> Chunk
chunkFromTexts = Chunk mempty

-- | Creates a 'Chunk' from a lazy 'XL.Text' with default colors and
-- no special effects.
chunkFromLazyText :: XL.Text -> Chunk
chunkFromLazyText = Chunk mempty . XL.toChunks

-- | Creates a 'Chunk' from a list of lazy 'XL.Text' with default
-- colors and no special effects.
chunkFromLazyTexts :: [XL.Text] -> Chunk
chunkFromLazyTexts = Chunk mempty . concatMap XL.toChunks

instance Monoid Chunk where
  mempty = Chunk mempty mempty
  mappend (Chunk s1 t1) (Chunk s2 t2) = Chunk (s1 <> s2) (t1 <> t2)


-- 8-color effects

bold8 :: Chunk
bold8 = x {
  chunkTextSpec = (chunkTextSpec x) {
    style8 = (style8 (chunkTextSpec x)) {
      common8 = (common8 (style8 (chunkTextSpec x))) {
        scBold = Last (Just True) }}}}
  where
    x = mempty

bold8off :: Chunk
bold8off = x {
  chunkTextSpec = (chunkTextSpec x) {
    style8 = (style8 (chunkTextSpec x)) {
      common8 = (common8 (style8 (chunkTextSpec x))) {
        scBold = Last (Just False) }}}}
  where
    x = mempty

faint8 :: Chunk
faint8 = x {
  chunkTextSpec = (chunkTextSpec x) {
    style8 = (style8 (chunkTextSpec x)) {
      common8 = (common8 (style8 (chunkTextSpec x))) {
        scFaint = Last (Just True) }}}}
  where
    x = mempty

faint8off :: Chunk
faint8off = x {
  chunkTextSpec = (chunkTextSpec x) {
    style8 = (style8 (chunkTextSpec x)) {
      common8 = (common8 (style8 (chunkTextSpec x))) {
        scFaint = Last (Just False) }}}}
  where
    x = mempty

italic8 :: Chunk
italic8 = x {
  chunkTextSpec = (chunkTextSpec x) {
    style8 = (style8 (chunkTextSpec x)) {
      common8 = (common8 (style8 (chunkTextSpec x))) {
        scItalic = Last (Just True) }}}}
  where
    x = mempty

italic8off :: Chunk
italic8off = x {
  chunkTextSpec = (chunkTextSpec x) {
    style8 = (style8 (chunkTextSpec x)) {
      common8 = (common8 (style8 (chunkTextSpec x))) {
        scItalic = Last (Just False) }}}}
  where
    x = mempty


underline8 :: Chunk
underline8 = x {
  chunkTextSpec = (chunkTextSpec x) {
    style8 = (style8 (chunkTextSpec x)) {
      common8 = (common8 (style8 (chunkTextSpec x))) {
        scUnderline = Last (Just True) }}}}
  where
    x = mempty


underline8off :: Chunk
underline8off = x {
  chunkTextSpec = (chunkTextSpec x) {
    style8 = (style8 (chunkTextSpec x)) {
      common8 = (common8 (style8 (chunkTextSpec x))) {
        scUnderline = Last (Just False) }}}}
  where
    x = mempty

blink8 :: Chunk
blink8 = x {
  chunkTextSpec = (chunkTextSpec x) {
    style8 = (style8 (chunkTextSpec x)) {
      common8 = (common8 (style8 (chunkTextSpec x))) {
        scBlink = Last (Just True) }}}}
  where
    x = mempty

blink8off :: Chunk
blink8off = x {
  chunkTextSpec = (chunkTextSpec x) {
    style8 = (style8 (chunkTextSpec x)) {
      common8 = (common8 (style8 (chunkTextSpec x))) {
        scBlink = Last (Just False) }}}}
  where
    x = mempty


inverse8 :: Chunk
inverse8 = x {
  chunkTextSpec = (chunkTextSpec x) {
    style8 = (style8 (chunkTextSpec x)) {
      common8 = (common8 (style8 (chunkTextSpec x))) {
        scInverse = Last (Just True) }}}}
  where
    x = mempty

inverse8off :: Chunk
inverse8off = x {
  chunkTextSpec = (chunkTextSpec x) {
    style8 = (style8 (chunkTextSpec x)) {
      common8 = (common8 (style8 (chunkTextSpec x))) {
        scInverse = Last (Just False) }}}}
  where
    x = mempty


invisible8 :: Chunk
invisible8 = x {
  chunkTextSpec = (chunkTextSpec x) {
    style8 = (style8 (chunkTextSpec x)) {
      common8 = (common8 (style8 (chunkTextSpec x))) {
        scInvisible = Last (Just True) }}}}
  where
    x = mempty

invisible8off :: Chunk
invisible8off = x {
  chunkTextSpec = (chunkTextSpec x) {
    style8 = (style8 (chunkTextSpec x)) {
      common8 = (common8 (style8 (chunkTextSpec x))) {
        scInvisible = Last (Just False) }}}}
  where
    x = mempty


strikeout8 :: Chunk
strikeout8 = x {
  chunkTextSpec = (chunkTextSpec x) {
    style8 = (style8 (chunkTextSpec x)) {
      common8 = (common8 (style8 (chunkTextSpec x))) {
        scStrikeout = Last (Just True) }}}}
  where
    x = mempty

strikeout8off :: Chunk
strikeout8off = x {
  chunkTextSpec = (chunkTextSpec x) {
    style8 = (style8 (chunkTextSpec x)) {
      common8 = (common8 (style8 (chunkTextSpec x))) {
        scStrikeout = Last (Just False) }}}}
  where
    x = mempty


-- 256 color effects

bold256 :: Chunk
bold256 = x {
  chunkTextSpec = (chunkTextSpec x) {
    style256 = (style256 (chunkTextSpec x)) {
      common256 = (common256 (style256 (chunkTextSpec x))) {
        scBold = Last (Just True) }}}}
  where
    x = mempty

bold256off :: Chunk
bold256off = x {
  chunkTextSpec = (chunkTextSpec x) {
    style256 = (style256 (chunkTextSpec x)) {
      common256 = (common256 (style256 (chunkTextSpec x))) {
        scBold = Last (Just False) }}}}
  where
    x = mempty


faint256 :: Chunk
faint256 = x {
  chunkTextSpec = (chunkTextSpec x) {
    style256 = (style256 (chunkTextSpec x)) {
      common256 = (common256 (style256 (chunkTextSpec x))) {
        scFaint = Last (Just True) }}}}
  where
    x = mempty

faint256off :: Chunk
faint256off = x {
  chunkTextSpec = (chunkTextSpec x) {
    style256 = (style256 (chunkTextSpec x)) {
      common256 = (common256 (style256 (chunkTextSpec x))) {
        scFaint = Last (Just False) }}}}
  where
    x = mempty


italic256 :: Chunk
italic256 = x {
  chunkTextSpec = (chunkTextSpec x) {
    style256 = (style256 (chunkTextSpec x)) {
      common256 = (common256 (style256 (chunkTextSpec x))) {
        scItalic = Last (Just True) }}}}
  where
    x = mempty

italic256off :: Chunk
italic256off = x {
  chunkTextSpec = (chunkTextSpec x) {
    style256 = (style256 (chunkTextSpec x)) {
      common256 = (common256 (style256 (chunkTextSpec x))) {
        scItalic = Last (Just False) }}}}
  where
    x = mempty


underline256 :: Chunk
underline256 = x {
  chunkTextSpec = (chunkTextSpec x) {
    style256 = (style256 (chunkTextSpec x)) {
      common256 = (common256 (style256 (chunkTextSpec x))) {
        scUnderline = Last (Just True) }}}}
  where
    x = mempty


underline256off :: Chunk
underline256off = x {
  chunkTextSpec = (chunkTextSpec x) {
    style256 = (style256 (chunkTextSpec x)) {
      common256 = (common256 (style256 (chunkTextSpec x))) {
        scUnderline = Last (Just False) }}}}
  where
    x = mempty

blink256 :: Chunk
blink256 = x {
  chunkTextSpec = (chunkTextSpec x) {
    style256 = (style256 (chunkTextSpec x)) {
      common256 = (common256 (style256 (chunkTextSpec x))) {
        scBlink = Last (Just True) }}}}
  where
    x = mempty


blink256off :: Chunk
blink256off = x {
  chunkTextSpec = (chunkTextSpec x) {
    style256 = (style256 (chunkTextSpec x)) {
      common256 = (common256 (style256 (chunkTextSpec x))) {
        scBlink = Last (Just False) }}}}
  where
    x = mempty


inverse256 :: Chunk
inverse256 = x {
  chunkTextSpec = (chunkTextSpec x) {
    style256 = (style256 (chunkTextSpec x)) {
      common256 = (common256 (style256 (chunkTextSpec x))) {
        scInverse = Last (Just True) }}}}
  where
    x = mempty

inverse256off :: Chunk
inverse256off = x {
  chunkTextSpec = (chunkTextSpec x) {
    style256 = (style256 (chunkTextSpec x)) {
      common256 = (common256 (style256 (chunkTextSpec x))) {
        scInverse = Last (Just False) }}}}
  where
    x = mempty


invisible256 :: Chunk
invisible256 = x {
  chunkTextSpec = (chunkTextSpec x) {
    style256 = (style256 (chunkTextSpec x)) {
      common256 = (common256 (style256 (chunkTextSpec x))) {
        scInvisible = Last (Just True) }}}}
  where
    x = mempty

invisible256off :: Chunk
invisible256off = x {
  chunkTextSpec = (chunkTextSpec x) {
    style256 = (style256 (chunkTextSpec x)) {
      common256 = (common256 (style256 (chunkTextSpec x))) {
        scInvisible = Last (Just False) }}}}
  where
    x = mempty


strikeout256 :: Chunk
strikeout256 = x {
  chunkTextSpec = (chunkTextSpec x) {
    style256 = (style256 (chunkTextSpec x)) {
      common256 = (common256 (style256 (chunkTextSpec x))) {
        scStrikeout = Last (Just True) }}}}
  where
    x = mempty

strikeout256off :: Chunk
strikeout256off = x {
  chunkTextSpec = (chunkTextSpec x) {
    style256 = (style256 (chunkTextSpec x)) {
      common256 = (common256 (style256 (chunkTextSpec x))) {
        scStrikeout = Last (Just False) }}}}
  where
    x = mempty



--
-- All
--


-- | Bold. What actually happens when you use Bold is going to depend
-- on your terminal. For example, xterm allows you actually use a bold
-- font for bold, if you have one. Otherwise, it might simulate bold
-- by using overstriking. Another possibility is that your terminal
-- might use a different color to indicate bold. For more details (at
-- least for xterm), look at xterm (1) and search for @boldColors@.
--
-- If your terminal uses a different color for bold, this allows an
-- 8-color terminal to really have 16 colors.
bold :: Chunk
bold = bold8 <> bold256

boldOff :: Chunk
boldOff = bold8off <> bold256off

faint :: Chunk
faint = faint8 <> faint256

faintOff :: Chunk
faintOff = faint8off <> faint256off

italic :: Chunk
italic = italic8 <> italic256

italicOff :: Chunk
italicOff = italic8off <> italic256off

underline :: Chunk
underline = underline8 <> underline256

underlineOff :: Chunk
underlineOff = underline8off <> underline256off

blink :: Chunk
blink = blink8 <> blink256

blinkOff :: Chunk
blinkOff = blink8off <> blink256off

inverse :: Chunk
inverse = inverse8 <> inverse256

inverseOff :: Chunk
inverseOff = inverse8off <> inverse256off

invisible :: Chunk
invisible = invisible8 <> invisible256

invisibleOff :: Chunk
invisibleOff = invisible8off <> invisible256off

strikeout :: Chunk
strikeout = strikeout8 <> strikeout256

strikeoutOff :: Chunk
strikeoutOff = strikeout8off <> strikeout256off

