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
  deriving (Eq, Ord, Show, Bounded, Enum)

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
  } deriving (Eq, Ord, Show)

-- | Color for an 256-color terminal.  Does not affect 8-color
-- terminals.

newtype Color256 = Color256
  { unColor256 :: Maybe Word8
  -- ^ Nothing indicates to use the default color for the terminal;
  -- otherwise, use the corresponding Terminfo 'T.Color'.
  } deriving (Eq, Ord, Show)

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
  , scUnderline :: Last Bool
  , scFlash :: Last Bool
  , scInverse :: Last Bool
  } deriving (Show, Eq, Ord)


instance Monoid StyleCommon where
  mempty = StyleCommon (Last Nothing) (Last Nothing)
                       (Last Nothing) (Last Nothing)
  mappend (StyleCommon b1 u1 f1 i1) (StyleCommon b2 u2 f2 i2)
    = StyleCommon (b1 <> b2) (u1 <> u2) (f1 <> f2) (i1 <> i2)

-- | Describes text appearance (foreground and background colors, as
-- well as other attributes such as bold) for an 8 color terminal.
data Style8 = Style8
  { foreground8 :: Foreground8
  , background8 :: Background8
  , common8 :: StyleCommon
  } deriving (Show, Eq, Ord)


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
  } deriving (Show, Eq, Ord)


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
  } deriving (Show, Eq, Ord)


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
--
-- The text is held as a list of strict 'Text'.

data Chunk = Chunk
  { textSpec :: TextSpec
  , text :: [Text]
  } deriving (Eq, Show, Ord)


instance Str.IsString Chunk where
  fromString s = Chunk mempty [(X.pack s)]

-- | Creates a 'Chunk' from a strict 'X.Text' with default colors
-- and no special effects.
fromText :: Text -> Chunk
fromText = Chunk mempty . (:[])

-- | Creates a 'Chunk' from a lazy 'XL.Text' with default colors and
-- no special effects.
fromLazyText :: XL.Text -> Chunk
fromLazyText = Chunk mempty . XL.toChunks

instance Monoid Chunk where
  mempty = Chunk mempty mempty
  mappend (Chunk s1 t1) (Chunk s2 t2) = Chunk (s1 <> s2) (t1 <> t2)


bold8 :: Chunk
bold8 = x {
  textSpec = (textSpec x) {
    style8 = (style8 (textSpec x)) {
      common8 = (common8 (style8 (textSpec x))) {
        scBold = Last (Just True) }}}}
  where
    x = mempty

bold8off :: Chunk
bold8off = x {
  textSpec = (textSpec x) {
    style8 = (style8 (textSpec x)) {
      common8 = (common8 (style8 (textSpec x))) {
        scBold = Last (Just False) }}}}
  where
    x = mempty


underline8 :: Chunk
underline8 = x {
  textSpec = (textSpec x) {
    style8 = (style8 (textSpec x)) {
      common8 = (common8 (style8 (textSpec x))) {
        scUnderline = Last (Just True) }}}}
  where
    x = mempty


underline8off :: Chunk
underline8off = x {
  textSpec = (textSpec x) {
    style8 = (style8 (textSpec x)) {
      common8 = (common8 (style8 (textSpec x))) {
        scUnderline = Last (Just False) }}}}
  where
    x = mempty

flash8 :: Chunk
flash8 = x {
  textSpec = (textSpec x) {
    style8 = (style8 (textSpec x)) {
      common8 = (common8 (style8 (textSpec x))) {
        scFlash = Last (Just True) }}}}
  where
    x = mempty

flash8off :: Chunk
flash8off = x {
  textSpec = (textSpec x) {
    style8 = (style8 (textSpec x)) {
      common8 = (common8 (style8 (textSpec x))) {
        scFlash = Last (Just False) }}}}
  where
    x = mempty


inverse8 :: Chunk
inverse8 = x {
  textSpec = (textSpec x) {
    style8 = (style8 (textSpec x)) {
      common8 = (common8 (style8 (textSpec x))) {
        scInverse = Last (Just True) }}}}
  where
    x = mempty

inverse8off :: Chunk
inverse8off = x {
  textSpec = (textSpec x) {
    style8 = (style8 (textSpec x)) {
      common8 = (common8 (style8 (textSpec x))) {
        scInverse = Last (Just False) }}}}
  where
    x = mempty


underline256 :: Chunk
underline256 = x {
  textSpec = (textSpec x) {
    style256 = (style256 (textSpec x)) {
      common256 = (common256 (style256 (textSpec x))) {
        scUnderline = Last (Just True) }}}}
  where
    x = mempty


underline256off :: Chunk
underline256off = x {
  textSpec = (textSpec x) {
    style256 = (style256 (textSpec x)) {
      common256 = (common256 (style256 (textSpec x))) {
        scUnderline = Last (Just False) }}}}
  where
    x = mempty

bold256 :: Chunk
bold256 = x {
  textSpec = (textSpec x) {
    style256 = (style256 (textSpec x)) {
      common256 = (common256 (style256 (textSpec x))) {
        scBold = Last (Just True) }}}}
  where
    x = mempty

bold256off :: Chunk
bold256off = x {
  textSpec = (textSpec x) {
    style256 = (style256 (textSpec x)) {
      common256 = (common256 (style256 (textSpec x))) {
        scBold = Last (Just False) }}}}
  where
    x = mempty


inverse256 :: Chunk
inverse256 = x {
  textSpec = (textSpec x) {
    style256 = (style256 (textSpec x)) {
      common256 = (common256 (style256 (textSpec x))) {
        scInverse = Last (Just True) }}}}
  where
    x = mempty

inverse256off :: Chunk
inverse256off = x {
  textSpec = (textSpec x) {
    style256 = (style256 (textSpec x)) {
      common256 = (common256 (style256 (textSpec x))) {
        scInverse = Last (Just False) }}}}
  where
    x = mempty


flash256 :: Chunk
flash256 = x {
  textSpec = (textSpec x) {
    style256 = (style256 (textSpec x)) {
      common256 = (common256 (style256 (textSpec x))) {
        scFlash = Last (Just True) }}}}
  where
    x = mempty


flash256off :: Chunk
flash256off = x {
  textSpec = (textSpec x) {
    style256 = (style256 (textSpec x)) {
      common256 = (common256 (style256 (textSpec x))) {
        scFlash = Last (Just False) }}}}
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

