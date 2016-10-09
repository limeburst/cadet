{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Ivory.Language
import qualified Ivory.Compile.C.CmdlineFrontend as C (compile)

key_none = 0x00
key_fn = 0xC0
key_left_ctrl = 0xE0
key_left_shift = 0xE1
key_left_alt = 0xE2
key_left_gui = 0xE3
key_right_ctrl = 0xE4
key_right_shift = 0xE5
key_right_alt = 0xE6
key_right_gui = 0xE7

[ivory|
struct KeyboardInputReport
  { modifier :: Stored Uint8
  ; reserved :: Stored Uint8
  ; keycode :: Array 6 (Stored Uint8)
  }
|]

report :: MemArea ('Struct "KeyboardInputReport")
report  = area "report" $ Just (istruct [])

is_modifier :: Def ('[Uint8] :-> IBool)
is_modifier = proc "is_modifier" $ \ key -> body $ do 
  ret (key >=? key_left_ctrl .&& key <=? key_right_gui)

matrix_changed :: Def ('[Ref s (Array 5 (Stored Uint16)),
                         Ref s (Array 5 (Stored Uint16))] :-> IBool)
matrix_changed = proc "matrix_changed" $ \ a b -> body $ do
  n <- local (ival (0 :: Uint8))
  arrayMap $ \ ix -> do
    aE <- deref (a ! ix)
    bE <- deref (b ! ix)
    ifte_ (aE /=? bE)
      (ret true)
      (do
        nn <- deref n
        store n nn)
  ret false

fn_pressed :: Def ('[Ref s (Array 5 (Stored Uint16)),
                     Ref s (Array 5 (Array 15 (Stored Uint8)))] :-> IBool)
fn_pressed = proc "fn_pressed" $ \ matrix keymap -> body $ do
  n <- local (ival (0 :: Uint8))
  arrayMap $ \ ix -> do
    arrayMap $ \ iy -> do
      m <- deref (matrix ! ix)
      k <- deref (keymap ! ix ! iy)
      ifte_ ((((m `iShiftR` (safeCast iy)) .& 1) ==? 1) .&& k ==? key_fn)
        (ret true)
        (do
          nn <- deref n
          store n nn)
  ret false

get_fn_key :: Def ('[Ref s (Array 2 (Array 5 (Array 15 (Stored Uint8)))),
                     Ix 5, Ix 15] :-> Uint8)
get_fn_key = proc "get_fn_key" $ \ keymap row col -> body $ do
  fn_layer_key <- deref (keymap ! 1 ! row ! col)
  default_layer_key <- deref (keymap ! 0 ! row ! col)
  ifte_ (fn_layer_key ==? 0)
    (ret default_layer_key)
    (ret fn_layer_key)

update_report :: Def ('[Ref s (Array 5 (Stored Uint16)),
                        Ref s (Array 2 (Array 5 (Array 15 (Stored Uint8))))]
                      :-> ())
update_report = proc "update_report" $ \ matrix keymap -> body $ do
  store ((addrOf report) ~> modifier) 0
  arrayMap $ \ ix -> do
    store (((addrOf report) ~> keycode) ! (ix :: Ix 6)) 0
  keys <- local (izero :: Init ('Stored Uint8))
  fp <- call fn_pressed matrix (keymap ! 0)
  arrayMap $ \ ix -> do
    arrayMap $ \ iy -> do
      m <- deref (matrix ! ix)
      ifte_ ((m `iShiftR` (safeCast iy) .& 1) ==? 1)
        (do
          key_ref <- local (izero :: Init ('Stored Uint8))
          fn_key <- call get_fn_key keymap ix iy
          normal_key <- deref (keymap ! 0 ! ix ! iy)
          ifte_ (fp)
            (store key_ref fn_key)
            (store key_ref normal_key)
          key <- deref key_ref
          current_modifier <- deref ((addrOf report) ~> modifier)
          is_modifier_key <- call is_modifier key
          ifte_ (is_modifier_key)
            (store
              ((addrOf report) ~> modifier)
              (current_modifier .| (1 `iShiftL` (key - 0xE0)))
            )
            (do 
              keys_val <- deref keys
              ifte_ (keys_val ==? 0)
                (store (((addrOf report) ~> keycode) ! 0 ) key)
                (store keys keys_val)
              ifte_ (keys_val ==? 1)
                (store (((addrOf report) ~> keycode) ! 1 ) key)
                (store keys keys_val)
              ifte_ (keys_val ==? 2)
                (store (((addrOf report) ~> keycode) ! 2 ) key)
                (store keys keys_val)
              ifte_ (keys_val ==? 3)
                (store (((addrOf report) ~> keycode) ! 3 ) key)
                (store keys keys_val)
              ifte_ (keys_val ==? 4)
                (store (((addrOf report) ~> keycode) ! 4 ) key)
                (store keys keys_val)
              ifte_ (keys_val ==? 5)
                (store (((addrOf report) ~> keycode) ! 5 ) key)
                (store keys keys_val)
              store keys (keys_val + 1)
            )
        )
        (do
          keys_val <- deref keys
          store keys keys_val)

cadet :: Module
cadet = package "cadet" $ do
  defMemArea report
  incl fn_pressed
  incl is_modifier
  incl update_report
  incl matrix_changed
  incl get_fn_key
  defStruct (Proxy :: Proxy "KeyboardInputReport")
