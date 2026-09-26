module Hashline = Crush_core.Hashline

let digest name expected data seed =
  Test_tools_test_support.case name `Quick (fun () ->
      Alcotest.check Alcotest.int32 name expected (Hashline.xxh32 data seed))

let normalized name expected data =
  Test_tools_test_support.case name `Quick (fun () ->
      Alcotest.check Alcotest.string name expected (Hashline.normalize data))

let tagged name expected data =
  Test_tools_test_support.case name `Quick (fun () ->
      Alcotest.check Alcotest.string name expected (Hashline.tag data))

let boundary_data =
  "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

let boundary_vectors =
  [
    (0, 0x02CC5D05l);
    (1, 0x48454CB2l);
    (2, 0x034D0471l);
    (3, 0x48009497l);
    (4, 0x0A0B4C93l);
    (5, 0x8AA3B71Cl);
    (6, 0x994E4577l);
    (7, 0x1907AD24l);
    (8, 0x189BBFBFl);
    (9, 0x0493D634l);
    (10, 0x950C9C0Al);
    (11, 0xD6509106l);
    (12, 0xBA508D84l);
    (13, 0x6BA381B4l);
    (14, 0xF45ACA85l);
    (15, 0x1DBDFA0Fl);
    (16, 0xC2C45B69l);
    (17, 0xCC79B217l);
    (18, 0x30EA5CD3l);
    (19, 0xA16E1F9Fl);
    (20, 0x35600916l);
    (21, 0xC392B286l);
    (22, 0x00F9D843l);
    (23, 0x191F4DC6l);
    (24, 0x5F247A24l);
    (25, 0x1DBBDA6Dl);
    (26, 0xB6E6AACCl);
    (27, 0xA5B2AF3El);
    (28, 0xAC152660l);
    (29, 0xA9E9C5BEl);
    (30, 0xAF94A08Al);
    (31, 0xA3944879l);
    (32, 0xEFB1272Dl);
    (33, 0x93234478l);
    (34, 0x6E821DDCl);
    (35, 0x297A4983l);
    (36, 0x9AA38E7El);
    (37, 0x1935DA71l);
    (38, 0xADD588B6l);
    (39, 0xC3AEB607l);
    (40, 0xF1A0F865l);
    (41, 0x1FAAB224l);
    (42, 0x40D6C830l);
    (43, 0x989FAB59l);
    (44, 0x84F01304l);
    (45, 0x82A092DDl);
    (46, 0x3F09DE14l);
    (47, 0x1A6C28CEl);
    (48, 0xB7F2B7A3l);
    (49, 0x456C52BFl);
    (50, 0xC01310D3l);
    (51, 0x4191B64Fl);
    (52, 0x42E233BAl);
    (53, 0x50EAEAD2l);
    (54, 0x3423994Dl);
    (55, 0xB6892B4El);
    (56, 0xB7A05C40l);
    (57, 0x94264F7Dl);
    (58, 0x98C50932l);
    (59, 0xA2B637DDl);
    (60, 0x4B27600Cl);
    (61, 0x5513EBA3l);
    (62, 0x2AEA96E9l);
    (63, 0x10BA0472l);
    (64, 0x83A6AA65l);
    (65, 0x2D98CA00l);
    (66, 0x1B3DD175l);
    (67, 0xC41D50CFl);
    (68, 0x509B1BE6l);
    (69, 0x87DF5E8Cl);
    (70, 0x0B8B8B89l);
    (71, 0x3680E43El);
    (72, 0x029FBFB5l);
  ]

let boundary_case =
  Test_tools_test_support.case "all byte lengths through four blocks" `Quick (fun () ->
      List.iter
        (fun (length, expected) ->
          let data = String.sub boundary_data 0 length in
          let name = "length " ^ string_of_int length in
          Alcotest.check Alcotest.int32 name expected (Hashline.xxh32 data 0l))
        boundary_vectors)

let cases =
  [
    digest "empty input" 0x02CC5D05l "" 0l;
    digest "single ASCII byte" 0x550D7456l "a" 0l;
    digest "ASCII triple" 0x32D153FFl "abc" 0l;
    digest "seed one" 0x0B2CB792l "" 1l;
    digest "prime seed" 0x36B78AE7l "" 0x9E3779B1l;
    digest "all-one seed" 0x9061DA9Dl "" 0xFFFFFFFFl;
    digest "seeded ASCII" 0x11364062l "abc" 0x12345678l;
    digest "exact sixteen-byte block" 0xC2C45B69l "0123456789abcdef" 0l;
    digest "one-byte partial tail" 0xCC79B217l "0123456789abcdefg" 0l;
    digest "three-byte partial tail" 0xA16E1F9Fl "0123456789abcdefghi" 0l;
    digest "four-byte partial tail" 0x35600916l "0123456789abcdefghij" 0l;
    digest "two sixteen-byte blocks" 0xEFB1272Dl "0123456789abcdefghijklmnopqrstuv" 0l;
    digest "binary ASCII-range bytes" 0xB72837F4l
      "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f" 0l;
    digest "binary mixed bytes" 0x25060A4Dl
      "\x00\xff\x80\x7f\x10\x00\xc3\xa9\xf0\x9f\x8c\x88" 0l;
    digest "UTF-8 bytes" 0x903F78E0l
      "\x68\xc3\xa9\x6c\x6c\x6f\x20\xe4\xb8\x96\xe7\x95\x8c\x20\xf0\x9f\x8c\x88\x0a" 0l;
    boundary_case;
    normalized "trailing whitespace on mixed lines" "a\n b\nc" "a \n b\t\r\nc";
    normalized "trailing whitespace on final line" "last" "last \t\r";
    normalized "newline remains after stripped line" "x\n" "x \n";
    normalized "empty lines remain" "\n\n" " \n\t\r\n";
    normalized "internal carriage return remains" "a\rb\n" "a\rb\r\n";
    normalized "empty input remains empty" "" "";
    tagged "empty tag" "5D05" "";
    tagged "hello newline tag" "5BF9" "hello\n";
    tagged "mixed-line tag" "80BA" "a \n b\t\r\nc";
    tagged "normalized mixed-line tag" "80BA" "a\n b\nc";
    tagged "trailing whitespace does not alter tag" "5BF9" "hello \n";
    tagged "missing final newline changes tag" "77F9" "hello";
  ]
