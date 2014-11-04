public class State_monad
{
  static boolean apply ()
  {
    class Let0
    {
      Object out;
      f2j.Closure x2;
      {
        class Fun56 extends f2j.Closure
        {
          f2j.Closure x57 = this;
          public void apply ()
          {
            final f2j.Closure x58 = (f2j.Closure) x57.x;
            class Fun59 extends f2j.Closure
            {
              f2j.Closure x60 = this;
              public void apply ()
              {
                final Object x61 = x60.x;
                class Fun62 extends f2j.Closure
                {
                  f2j.Closure x63 = this;
                  public void apply ()
                  {
                    final f2j.FunctionalList x64 = (f2j.FunctionalList) x63.x;
                    final java.lang.Boolean x66 = x64.isEmpty();
                    Object ifres65;
                    if (x66)
                    {
                      ifres65 = x61;
                    }
                    else
                    {
                      final f2j.Closure x70 = x58;
                      class Fun72 extends f2j.Closure
                      {
                        f2j.Closure x73 = this;
                        public void apply ()
                        {
                          final Object x74 = x73.x;
                          final Object x78 = x74;
                          final Object x79 = x78;
                          f2j.Closure x77 = x70;
                          x77.x = x79;
                          x77.apply();
                          final f2j.Closure temp80 = (f2j.Closure) x77.out;
                          final f2j.Closure x75 = temp80;
                          class Fun81 extends f2j.Closure
                          {
                            f2j.Closure x82 = this;
                            public void apply ()
                            {
                              final java.lang.Integer x83 = (java.lang.Integer) x82.x;
                              final java.lang.Integer x87 = x83;
                              final java.lang.Integer x88 = x87;
                              f2j.Closure x86 = x75;
                              x86.x = x88;
                              x86.apply();
                              final Object temp89 = x86.out;
                              final Object x84 = temp89;
                              final Object x85 = x84;
                              out = x85;
                            }
                          }
                          f2j.Closure x81 = new Fun81();
                          final f2j.Closure x76 = x81;
                          out = x76;
                        }
                      }
                      f2j.Closure x72 = new Fun72();
                      final f2j.Closure x71 = x72;
                      f2j.Closure x69 = x2;
                      x69.x = x71;
                      x69.apply();
                      final f2j.Closure temp90 = (f2j.Closure) x69.out;
                      final Object x95 = x61;
                      final Object x96 = x95;
                      f2j.Closure x94 = x58;
                      x94.x = x96;
                      x94.apply();
                      final f2j.Closure temp97 = (f2j.Closure) x94.out;
                      final java.lang.Integer x100 = x64.head();
                      final java.lang.Integer x98 = x100;
                      final java.lang.Integer x99 = x98;
                      f2j.Closure x93 = temp97;
                      x93.x = x99;
                      x93.apply();
                      final Object temp101 = x93.out;
                      final Object x91 = temp101;
                      final Object x92 = x91;
                      f2j.Closure x68 = temp90;
                      x68.x = x92;
                      x68.apply();
                      final f2j.Closure temp102 = (f2j.Closure) x68.out;
                      final f2j.FunctionalList x105 = x64.tail();
                      final f2j.FunctionalList x103 = x105;
                      final f2j.FunctionalList x104 = x103;
                      f2j.Closure x67 = temp102;
                      x67.x = x104;
                      x67.apply();
                      final Object temp106 = x67.out;
                      ifres65 = temp106;
                    }
                    out = ifres65;
                  }
                }
                f2j.Closure x62 = new Fun62();
                out = x62;
              }
            }
            f2j.Closure x59 = new Fun59();
            out = x59;
          }
        }
        f2j.Closure x56 = new Fun56();
        x2 = x56;
        class Fun111 extends f2j.Closure
        {
          f2j.Closure x112 = this;
          public void apply ()
          {
            final Object x113 = x112.x;
            class Fun114 extends f2j.Closure
            {
              f2j.Closure x115 = this;
              public void apply ()
              {
                final Object x116 = x115.x;
                final f2j.tuples.Tuple2 x117 = new f2j.tuples.Tuple2(x113, x116);
                out = x117;
              }
            }
            f2j.Closure x114 = new Fun114();
            out = x114;
          }
        }
        f2j.Closure x111 = new Fun111();
        class Fun121 extends f2j.Closure
        {
          f2j.Closure x122 = this;
          public void apply ()
          {
            final f2j.Closure x123 = (f2j.Closure) x122.x;
            class Fun124 extends f2j.Closure
            {
              f2j.Closure x125 = this;
              public void apply ()
              {
                final f2j.Closure x126 = (f2j.Closure) x125.x;
                class Fun127 extends f2j.Closure
                {
                  f2j.Closure x128 = this;
                  public void apply ()
                  {
                    final Object x129 = x128.x;
                    final Object x133 = x129;
                    final Object x134 = x133;
                    f2j.Closure x132 = x123;
                    x132.x = x134;
                    x132.apply();
                    final f2j.tuples.Tuple2 temp135 = (f2j.tuples.Tuple2) x132.out;
                    final f2j.tuples.Tuple2 x130 = temp135;
                    final Object x140 = (Object) x130._1;
                    final Object x138 = x140;
                    final Object x139 = x138;
                    f2j.Closure x137 = x126;
                    x137.x = x139;
                    x137.apply();
                    final f2j.Closure temp141 = (f2j.Closure) x137.out;
                    final Object x144 = (Object) x130._2;
                    final Object x142 = x144;
                    final Object x143 = x142;
                    f2j.Closure x136 = temp141;
                    x136.x = x143;
                    x136.apply();
                    final f2j.tuples.Tuple2 temp145 = (f2j.tuples.Tuple2) x136.out;
                    final f2j.tuples.Tuple2 x131 = temp145;
                    out = x131;
                  }
                }
                f2j.Closure x127 = new Fun127();
                out = x127;
              }
            }
            f2j.Closure x124 = new Fun124();
            out = x124;
          }
        }
        f2j.Closure x121 = new Fun121();
        class Fun147 extends f2j.Closure
        {
          f2j.Closure x148 = this;
          public void apply ()
          {
            final Object x149 = x148.x;
            final f2j.tuples.Tuple2 x150 = new f2j.tuples.Tuple2(x149, x149);
            out = x150;
          }
        }
        f2j.Closure x147 = new Fun147();
        class Fun152 extends f2j.Closure
        {
          f2j.Closure x153 = this;
          public void apply ()
          {
            final Object x154 = x153.x;
            class Fun155 extends f2j.Closure
            {
              f2j.Closure x156 = this;
              public void apply ()
              {
                final Object x157 = x156.x;
                final f2j.tuples.Tuple2 x158 = new f2j.tuples.Tuple2(0, x154);
                out = x158;
              }
            }
            f2j.Closure x155 = new Fun155();
            out = x155;
          }
        }
        f2j.Closure x152 = new Fun152();
        class Fun159 extends f2j.Closure
        {
          f2j.Closure x160 = this;
          public void apply ()
          {
            final java.lang.Integer x161 = (java.lang.Integer) x160.x;
            class Fun162 extends f2j.Closure
            {
              f2j.Closure x163 = this;
              public void apply ()
              {
                final f2j.FunctionalList x164 = (f2j.FunctionalList) x163.x;
                final f2j.FunctionalList x165 = new <java.lang.Integer, f2j.FunctionalList> f2j.FunctionalList(x161, x164);
                out = x165;
              }
            }
            f2j.Closure x162 = new Fun162();
            out = x162;
          }
        }
        f2j.Closure x159 = new Fun159();
        final f2j.tuples.Tuple5 x166 = new f2j.tuples.Tuple5(x111, x121, x147, x152, x159);
        final f2j.tuples.Tuple5 x107 = x166;
        class Fun169 extends f2j.Closure
        {
          f2j.Closure x170 = this;
          public void apply ()
          {
            final f2j.FunctionalList x171 = (f2j.FunctionalList) x170.x;
            class Fun174 extends f2j.Closure
            {
              f2j.Closure x175 = this;
              public void apply ()
              {
                final f2j.Closure x176 = (f2j.Closure) x175.x;
                class Fun177 extends f2j.Closure
                {
                  f2j.Closure x178 = this;
                  public void apply ()
                  {
                    final java.lang.Integer x179 = (java.lang.Integer) x178.x;
                    final f2j.Closure x182 = (f2j.Closure) x107._2;
                    final f2j.Closure x183 = x176;
                    class Fun185 extends f2j.Closure
                    {
                      f2j.Closure x186 = this;
                      public void apply ()
                      {
                        final java.lang.Integer x187 = (java.lang.Integer) x186.x;
                        final java.lang.Integer x191 = x187;
                        final java.lang.Integer x192 = x191;
                        f2j.Closure x190 = x183;
                        x190.x = x192;
                        x190.apply();
                        final f2j.tuples.Tuple2 temp193 = (f2j.tuples.Tuple2) x190.out;
                        final f2j.tuples.Tuple2 x188 = temp193;
                        final f2j.FunctionalList x196 = (f2j.FunctionalList) x188._1;
                        final f2j.FunctionalList x194 = x196;
                        final f2j.FunctionalList x195 = x194;
                        final java.lang.Integer x199 = (java.lang.Integer) x188._2;
                        final java.lang.Integer x197 = x199;
                        final java.lang.Integer x198 = x197;
                        final f2j.tuples.Tuple2 x200 = new f2j.tuples.Tuple2(x195, x198);
                        final f2j.tuples.Tuple2 x189 = x200;
                        out = x189;
                      }
                    }
                    f2j.Closure x185 = new Fun185();
                    final f2j.Closure x184 = x185;
                    f2j.Closure x181 = x182;
                    x181.x = x184;
                    x181.apply();
                    final f2j.Closure temp201 = (f2j.Closure) x181.out;
                    class Fun204 extends f2j.Closure
                    {
                      f2j.Closure x205 = this;
                      public void apply ()
                      {
                        final f2j.FunctionalList x206 = (f2j.FunctionalList) x205.x;
                        final f2j.Closure x209 = (f2j.Closure) x107._2;
                        final f2j.Closure x212 = (f2j.Closure) x107._3;
                        final f2j.Closure x210 = x212;
                        class Fun213 extends f2j.Closure
                        {
                          f2j.Closure x214 = this;
                          public void apply ()
                          {
                            final java.lang.Integer x215 = (java.lang.Integer) x214.x;
                            final java.lang.Integer x219 = x215;
                            final java.lang.Integer x220 = x219;
                            f2j.Closure x218 = x210;
                            x218.x = x220;
                            x218.apply();
                            final f2j.tuples.Tuple2 temp221 = (f2j.tuples.Tuple2) x218.out;
                            final f2j.tuples.Tuple2 x216 = temp221;
                            final Object x224 = (Object) x216._1;
                            final Object x222 = x224;
                            final Object x223 = x222;
                            final Object x227 = (Object) x216._2;
                            final Object x225 = x227;
                            final Object x226 = x225;
                            final f2j.tuples.Tuple2 x228 = new f2j.tuples.Tuple2(x223, x226);
                            final f2j.tuples.Tuple2 x217 = x228;
                            out = x217;
                          }
                        }
                        f2j.Closure x213 = new Fun213();
                        final f2j.Closure x211 = x213;
                        f2j.Closure x208 = x209;
                        x208.x = x211;
                        x208.apply();
                        final f2j.Closure temp229 = (f2j.Closure) x208.out;
                        class Fun232 extends f2j.Closure
                        {
                          f2j.Closure x233 = this;
                          public void apply ()
                          {
                            final java.lang.Integer x234 = (java.lang.Integer) x233.x;
                            final f2j.Closure x237 = (f2j.Closure) x107._2;
                            final f2j.Closure x241 = (f2j.Closure) x107._4;
                            final java.lang.Integer x244 = x234 + 1;
                            final java.lang.Integer x242 = x244;
                            final java.lang.Integer x243 = x242;
                            f2j.Closure x240 = x241;
                            x240.x = x243;
                            x240.apply();
                            final f2j.Closure temp245 = (f2j.Closure) x240.out;
                            final f2j.Closure x238 = temp245;
                            class Fun246 extends f2j.Closure
                            {
                              f2j.Closure x247 = this;
                              public void apply ()
                              {
                                final java.lang.Integer x248 = (java.lang.Integer) x247.x;
                                final java.lang.Integer x252 = x248;
                                final java.lang.Integer x253 = x252;
                                f2j.Closure x251 = x238;
                                x251.x = x253;
                                x251.apply();
                                final f2j.tuples.Tuple2 temp254 = (f2j.tuples.Tuple2) x251.out;
                                final f2j.tuples.Tuple2 x249 = temp254;
                                final java.lang.Integer x257 = (java.lang.Integer) x249._1;
                                final java.lang.Integer x255 = x257;
                                final java.lang.Integer x256 = x255;
                                final Object x260 = (Object) x249._2;
                                final Object x258 = x260;
                                final Object x259 = x258;
                                final f2j.tuples.Tuple2 x261 = new f2j.tuples.Tuple2(x256, x259);
                                final f2j.tuples.Tuple2 x250 = x261;
                                out = x250;
                              }
                            }
                            f2j.Closure x246 = new Fun246();
                            final f2j.Closure x239 = x246;
                            f2j.Closure x236 = x237;
                            x236.x = x239;
                            x236.apply();
                            final f2j.Closure temp262 = (f2j.Closure) x236.out;
                            class Fun265 extends f2j.Closure
                            {
                              f2j.Closure x266 = this;
                              public void apply ()
                              {
                                final java.lang.Integer x267 = (java.lang.Integer) x266.x;
                                final f2j.Closure x269 = (f2j.Closure) x107._1;
                                final f2j.Closure x274 = (f2j.Closure) x107._5;
                                final java.lang.Integer x275 = x234;
                                final java.lang.Integer x276 = x275;
                                f2j.Closure x273 = x274;
                                x273.x = x276;
                                x273.apply();
                                final f2j.Closure temp277 = (f2j.Closure) x273.out;
                                final f2j.FunctionalList x278 = x206;
                                final f2j.FunctionalList x279 = x278;
                                f2j.Closure x272 = temp277;
                                x272.x = x279;
                                x272.apply();
                                final f2j.FunctionalList temp280 = (f2j.FunctionalList) x272.out;
                                final f2j.FunctionalList x270 = temp280;
                                final f2j.FunctionalList x271 = x270;
                                f2j.Closure x268 = x269;
                                x268.x = x271;
                                x268.apply();
                                final f2j.Closure temp281 = (f2j.Closure) x268.out;
                                out = temp281;
                              }
                            }
                            f2j.Closure x265 = new Fun265();
                            final f2j.Closure x263 = x265;
                            class Fun282 extends f2j.Closure
                            {
                              f2j.Closure x283 = this;
                              public void apply ()
                              {
                                final java.lang.Integer x284 = (java.lang.Integer) x283.x;
                                final java.lang.Integer x288 = x284;
                                final java.lang.Integer x289 = x288;
                                f2j.Closure x287 = x263;
                                x287.x = x289;
                                x287.apply();
                                final f2j.Closure temp290 = (f2j.Closure) x287.out;
                                final f2j.Closure x285 = temp290;
                                class Fun291 extends f2j.Closure
                                {
                                  f2j.Closure x292 = this;
                                  public void apply ()
                                  {
                                    final java.lang.Integer x293 = (java.lang.Integer) x292.x;
                                    final java.lang.Integer x297 = x293;
                                    final java.lang.Integer x298 = x297;
                                    f2j.Closure x296 = x285;
                                    x296.x = x298;
                                    x296.apply();
                                    final f2j.tuples.Tuple2 temp299 = (f2j.tuples.Tuple2) x296.out;
                                    final f2j.tuples.Tuple2 x294 = temp299;
                                    final Object x302 = (Object) x294._1;
                                    final Object x300 = x302;
                                    final Object x301 = x300;
                                    final Object x305 = (Object) x294._2;
                                    final Object x303 = x305;
                                    final Object x304 = x303;
                                    final f2j.tuples.Tuple2 x306 = new f2j.tuples.Tuple2(x301, x304);
                                    final f2j.tuples.Tuple2 x295 = x306;
                                    out = x295;
                                  }
                                }
                                f2j.Closure x291 = new Fun291();
                                final f2j.Closure x286 = x291;
                                out = x286;
                              }
                            }
                            f2j.Closure x282 = new Fun282();
                            final f2j.Closure x264 = x282;
                            f2j.Closure x235 = temp262;
                            x235.x = x264;
                            x235.apply();
                            final f2j.Closure temp307 = (f2j.Closure) x235.out;
                            out = temp307;
                          }
                        }
                        f2j.Closure x232 = new Fun232();
                        final f2j.Closure x230 = x232;
                        class Fun308 extends f2j.Closure
                        {
                          f2j.Closure x309 = this;
                          public void apply ()
                          {
                            final java.lang.Integer x310 = (java.lang.Integer) x309.x;
                            final java.lang.Integer x314 = x310;
                            final java.lang.Integer x315 = x314;
                            f2j.Closure x313 = x230;
                            x313.x = x315;
                            x313.apply();
                            final f2j.Closure temp316 = (f2j.Closure) x313.out;
                            final f2j.Closure x311 = temp316;
                            class Fun317 extends f2j.Closure
                            {
                              f2j.Closure x318 = this;
                              public void apply ()
                              {
                                final java.lang.Integer x319 = (java.lang.Integer) x318.x;
                                final java.lang.Integer x323 = x319;
                                final java.lang.Integer x324 = x323;
                                f2j.Closure x322 = x311;
                                x322.x = x324;
                                x322.apply();
                                final f2j.tuples.Tuple2 temp325 = (f2j.tuples.Tuple2) x322.out;
                                final f2j.tuples.Tuple2 x320 = temp325;
                                final Object x328 = (Object) x320._1;
                                final Object x326 = x328;
                                final Object x327 = x326;
                                final Object x331 = (Object) x320._2;
                                final Object x329 = x331;
                                final Object x330 = x329;
                                final f2j.tuples.Tuple2 x332 = new f2j.tuples.Tuple2(x327, x330);
                                final f2j.tuples.Tuple2 x321 = x332;
                                out = x321;
                              }
                            }
                            f2j.Closure x317 = new Fun317();
                            final f2j.Closure x312 = x317;
                            out = x312;
                          }
                        }
                        f2j.Closure x308 = new Fun308();
                        final f2j.Closure x231 = x308;
                        f2j.Closure x207 = temp229;
                        x207.x = x231;
                        x207.apply();
                        final f2j.Closure temp333 = (f2j.Closure) x207.out;
                        out = temp333;
                      }
                    }
                    f2j.Closure x204 = new Fun204();
                    final f2j.Closure x202 = x204;
                    class Fun334 extends f2j.Closure
                    {
                      f2j.Closure x335 = this;
                      public void apply ()
                      {
                        final f2j.FunctionalList x336 = (f2j.FunctionalList) x335.x;
                        final f2j.FunctionalList x340 = x336;
                        final f2j.FunctionalList x341 = x340;
                        f2j.Closure x339 = x202;
                        x339.x = x341;
                        x339.apply();
                        final f2j.Closure temp342 = (f2j.Closure) x339.out;
                        final f2j.Closure x337 = temp342;
                        class Fun343 extends f2j.Closure
                        {
                          f2j.Closure x344 = this;
                          public void apply ()
                          {
                            final java.lang.Integer x345 = (java.lang.Integer) x344.x;
                            final java.lang.Integer x349 = x345;
                            final java.lang.Integer x350 = x349;
                            f2j.Closure x348 = x337;
                            x348.x = x350;
                            x348.apply();
                            final f2j.tuples.Tuple2 temp351 = (f2j.tuples.Tuple2) x348.out;
                            final f2j.tuples.Tuple2 x346 = temp351;
                            final Object x354 = (Object) x346._1;
                            final Object x352 = x354;
                            final Object x353 = x352;
                            final Object x357 = (Object) x346._2;
                            final Object x355 = x357;
                            final Object x356 = x355;
                            final f2j.tuples.Tuple2 x358 = new f2j.tuples.Tuple2(x353, x356);
                            final f2j.tuples.Tuple2 x347 = x358;
                            out = x347;
                          }
                        }
                        f2j.Closure x343 = new Fun343();
                        final f2j.Closure x338 = x343;
                        out = x338;
                      }
                    }
                    f2j.Closure x334 = new Fun334();
                    final f2j.Closure x203 = x334;
                    f2j.Closure x180 = temp201;
                    x180.x = x203;
                    x180.apply();
                    final f2j.Closure temp359 = (f2j.Closure) x180.out;
                    out = temp359;
                  }
                }
                f2j.Closure x177 = new Fun177();
                out = x177;
              }
            }
            f2j.Closure x174 = new Fun174();
            final f2j.Closure x172 = x174;
            class Fun362 extends f2j.Closure
            {
              f2j.Closure x363 = this;
              public void apply ()
              {
                final f2j.FunctionalList x364 = (f2j.FunctionalList) x363.x;
                final f2j.Closure x368 = x172;
                class Fun370 extends f2j.Closure
                {
                  f2j.Closure x371 = this;
                  public void apply ()
                  {
                    final f2j.Closure x372 = (f2j.Closure) x371.x;
                    final f2j.Closure x376 = x372;
                    class Fun378 extends f2j.Closure
                    {
                      f2j.Closure x379 = this;
                      public void apply ()
                      {
                        final java.lang.Integer x380 = (java.lang.Integer) x379.x;
                        final java.lang.Integer x384 = x380;
                        final java.lang.Integer x385 = x384;
                        f2j.Closure x383 = x376;
                        x383.x = x385;
                        x383.apply();
                        final f2j.tuples.Tuple2 temp386 = (f2j.tuples.Tuple2) x383.out;
                        final f2j.tuples.Tuple2 x381 = temp386;
                        final f2j.FunctionalList x389 = (f2j.FunctionalList) x381._1;
                        final f2j.FunctionalList x387 = x389;
                        final f2j.FunctionalList x388 = x387;
                        final java.lang.Integer x392 = (java.lang.Integer) x381._2;
                        final java.lang.Integer x390 = x392;
                        final java.lang.Integer x391 = x390;
                        final f2j.tuples.Tuple2 x393 = new f2j.tuples.Tuple2(x388, x391);
                        final f2j.tuples.Tuple2 x382 = x393;
                        out = x382;
                      }
                    }
                    f2j.Closure x378 = new Fun378();
                    final f2j.Closure x377 = x378;
                    f2j.Closure x375 = x368;
                    x375.x = x377;
                    x375.apply();
                    final f2j.Closure temp394 = (f2j.Closure) x375.out;
                    final f2j.Closure x373 = temp394;
                    class Fun395 extends f2j.Closure
                    {
                      f2j.Closure x396 = this;
                      public void apply ()
                      {
                        final java.lang.Integer x397 = (java.lang.Integer) x396.x;
                        final java.lang.Integer x401 = x397;
                        final java.lang.Integer x402 = x401;
                        f2j.Closure x400 = x373;
                        x400.x = x402;
                        x400.apply();
                        final f2j.Closure temp403 = (f2j.Closure) x400.out;
                        final f2j.Closure x398 = temp403;
                        class Fun404 extends f2j.Closure
                        {
                          f2j.Closure x405 = this;
                          public void apply ()
                          {
                            final java.lang.Integer x406 = (java.lang.Integer) x405.x;
                            final java.lang.Integer x410 = x406;
                            final java.lang.Integer x411 = x410;
                            f2j.Closure x409 = x398;
                            x409.x = x411;
                            x409.apply();
                            final f2j.tuples.Tuple2 temp412 = (f2j.tuples.Tuple2) x409.out;
                            final f2j.tuples.Tuple2 x407 = temp412;
                            final Object x415 = (Object) x407._1;
                            final Object x413 = x415;
                            final Object x414 = x413;
                            final Object x418 = (Object) x407._2;
                            final Object x416 = x418;
                            final Object x417 = x416;
                            final f2j.tuples.Tuple2 x419 = new f2j.tuples.Tuple2(x414, x417);
                            final f2j.tuples.Tuple2 x408 = x419;
                            out = x408;
                          }
                        }
                        f2j.Closure x404 = new Fun404();
                        final f2j.Closure x399 = x404;
                        out = x399;
                      }
                    }
                    f2j.Closure x395 = new Fun395();
                    final f2j.Closure x374 = x395;
                    out = x374;
                  }
                }
                f2j.Closure x370 = new Fun370();
                final f2j.Closure x369 = x370;
                f2j.Closure x367 = x2;
                x367.x = x369;
                x367.apply();
                final f2j.Closure temp420 = (f2j.Closure) x367.out;
                final f2j.Closure x424 = (f2j.Closure) x107._1;
                final f2j.FunctionalList x427 = new f2j.FunctionalList();
                final f2j.FunctionalList x425 = x427;
                final f2j.FunctionalList x426 = x425;
                f2j.Closure x423 = x424;
                x423.x = x426;
                x423.apply();
                final f2j.Closure temp428 = (f2j.Closure) x423.out;
                final f2j.Closure x421 = temp428;
                class Fun429 extends f2j.Closure
                {
                  f2j.Closure x430 = this;
                  public void apply ()
                  {
                    final java.lang.Integer x431 = (java.lang.Integer) x430.x;
                    final java.lang.Integer x435 = x431;
                    final java.lang.Integer x436 = x435;
                    f2j.Closure x434 = x421;
                    x434.x = x436;
                    x434.apply();
                    final f2j.tuples.Tuple2 temp437 = (f2j.tuples.Tuple2) x434.out;
                    final f2j.tuples.Tuple2 x432 = temp437;
                    final Object x440 = (Object) x432._1;
                    final Object x438 = x440;
                    final Object x439 = x438;
                    final Object x443 = (Object) x432._2;
                    final Object x441 = x443;
                    final Object x442 = x441;
                    final f2j.tuples.Tuple2 x444 = new f2j.tuples.Tuple2(x439, x442);
                    final f2j.tuples.Tuple2 x433 = x444;
                    out = x433;
                  }
                }
                f2j.Closure x429 = new Fun429();
                final f2j.Closure x422 = x429;
                f2j.Closure x366 = temp420;
                x366.x = x422;
                x366.apply();
                final f2j.Closure temp445 = (f2j.Closure) x366.out;
                final f2j.FunctionalList x446 = x364;
                final f2j.FunctionalList x447 = x446;
                f2j.Closure x365 = temp445;
                x365.x = x447;
                x365.apply();
                final f2j.Closure temp448 = (f2j.Closure) x365.out;
                out = temp448;
              }
            }
            f2j.Closure x362 = new Fun362();
            final f2j.Closure x360 = x362;
            final f2j.FunctionalList x451 = x171;
            final f2j.FunctionalList x452 = x451;
            f2j.Closure x450 = x360;
            x450.x = x452;
            x450.apply();
            final f2j.Closure temp453 = (f2j.Closure) x450.out;
            f2j.Closure x449 = temp453;
            x449.x = 0;
            x449.apply();
            final f2j.tuples.Tuple2 temp454 = (f2j.tuples.Tuple2) x449.out;
            final f2j.FunctionalList x455 = (f2j.FunctionalList) temp454._1;
            final f2j.FunctionalList x361 = x455;
            final f2j.FunctionalList x173 = x361;
            out = x173;
          }
        }
        f2j.Closure x169 = new Fun169();
        final f2j.Closure x167 = x169;
        class Fun461 extends f2j.Closure
        {
          f2j.Closure x460 = this;
          public void apply ()
          {
            final f2j.FunctionalList x462 = (f2j.FunctionalList) x460.x;
            final java.lang.Boolean x464 = x462.isEmpty();
            java.lang.Boolean ifres463;
            if (x464)
            {
              final java.io.PrintStream x465 = (java.io.PrintStream) java.lang.System.out;
              x465.<java.lang.String>println("");
              ifres463 = true;
            }
            else
            {
              final java.lang.Integer x466 = x462.head();
              final java.io.PrintStream x467 = (java.io.PrintStream) java.lang.System.out;
              x467.<java.lang.Integer>print(x466);
              final f2j.FunctionalList x471 = x462.tail();
              final f2j.FunctionalList x469 = x471;
              final f2j.FunctionalList x470 = x469;
              f2j.Closure x468 = x460;
              x468.x = x470;
              x468.apply();
              final java.lang.Boolean temp472 = (java.lang.Boolean) x468.out;
              ifres463 = false;
            }
            out = ifres463;
          }
        }
        f2j.Closure x461 = new Fun461();
        final f2j.Closure x458 = x461;
        class Fun473 extends f2j.Closure
        {
          f2j.Closure x474 = this;
          public void apply ()
          {
            final f2j.FunctionalList x475 = (f2j.FunctionalList) x474.x;
            final f2j.FunctionalList x479 = x475;
            final f2j.FunctionalList x480 = x479;
            f2j.Closure x478 = x458;
            x478.x = x480;
            x478.apply();
            final java.lang.Boolean temp481 = (java.lang.Boolean) x478.out;
            final java.lang.Boolean x476 = temp481;
            final java.lang.Boolean x477 = x476;
            out = x477;
          }
        }
        f2j.Closure x473 = new Fun473();
        final f2j.Closure x459 = x473;
        final f2j.Closure x456 = x459;
        final f2j.FunctionalList x488 = new f2j.FunctionalList();
        final f2j.FunctionalList x489 = new <java.lang.Integer, f2j.FunctionalList> f2j.FunctionalList(0, x488);
        final f2j.FunctionalList x490 = new <java.lang.Integer, f2j.FunctionalList> f2j.FunctionalList(0, x489);
        final f2j.FunctionalList x491 = new <java.lang.Integer, f2j.FunctionalList> f2j.FunctionalList(0, x490);
        final f2j.FunctionalList x492 = new <java.lang.Integer, f2j.FunctionalList> f2j.FunctionalList(0, x491);
        final f2j.FunctionalList x493 = new <java.lang.Integer, f2j.FunctionalList> f2j.FunctionalList(0, x492);
        final f2j.FunctionalList x494 = new <java.lang.Integer, f2j.FunctionalList> f2j.FunctionalList(0, x493);
        final f2j.FunctionalList x486 = x494;
        final f2j.FunctionalList x487 = x486;
        f2j.Closure x485 = x167;
        x485.x = x487;
        x485.apply();
        final f2j.FunctionalList temp495 = (f2j.FunctionalList) x485.out;
        final f2j.FunctionalList x483 = temp495;
        final f2j.FunctionalList x484 = x483;
        f2j.Closure x482 = x456;
        x482.x = x484;
        x482.apply();
        final java.lang.Boolean temp496 = (java.lang.Boolean) x482.out;
        final java.lang.Boolean x457 = temp496;
        final java.lang.Boolean x168 = x457;
        final java.lang.Boolean x108 = x168;
        out = x108;
      }
    }
    Let0 x0 = new Let0();
    final java.lang.Boolean x1 = (java.lang.Boolean) x0.out;
    return x1;
  }
  public static void main (String[] args)
  {
    System.out.println(apply());
  }
}