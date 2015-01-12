public class Mixin
{
  static int apply ()
  {
    class Fun1 extends f2j.Closure
    {
      f2j.Closure x2 = this;
      public void apply ()
      {
        final f2j.Closure x3 = (f2j.Closure) x2.arg;
        class Fun4 extends f2j.Closure
        {
          f2j.Closure x5 = this;
          public void apply ()
          {
            final f2j.Closure x6 = (f2j.Closure) x5.arg;
            res = x3;
          }
        }
        res = new Fun4();
      }
    }
    class Let7
    {
      Object temp;
      f2j.Closure x9;
      {
        class Fun100 extends f2j.Closure
        {
          f2j.Closure x101 = this;
          public void apply ()
          {
            final f2j.Closure x102 = (f2j.Closure) x101.arg;
            final f2j.Closure x103 = x9;
            class Fun147 extends f2j.Closure
            {
              f2j.Closure x148 = this;
              public void apply ()
              {
                final java.lang.Integer x149 = (java.lang.Integer) x148.arg;
                final f2j.Closure x150 = x102;
                class Fun151 extends f2j.Closure
                {
                  f2j.Closure x152 = this;
                  public void apply ()
                  {
                    final f2j.Closure x153 = (f2j.Closure) x152.arg;
                    final f2j.Closure x154 = x153;
                    class Fun155 extends f2j.Closure
                    {
                      f2j.Closure x156 = this;
                      public void apply ()
                      {
                        final java.lang.Integer x157 = (java.lang.Integer) x156.arg;
                        final java.lang.Integer x158 = x157;
                        f2j.Closure x159 = x154;
                        x159.arg = x158;
                        x159.apply();
                        final Object x160 = x159.res;
                        final Object x161 = x160;
                        res = x161;
                      }
                    }
                    f2j.Closure x162 = x150;
                    x162.arg = new Fun155();
                    x162.apply();
                    final f2j.Closure x163 = (f2j.Closure) x162.res;
                    final f2j.Closure x164 = x163;
                    class Fun165 extends f2j.Closure
                    {
                      f2j.Closure x166 = this;
                      public void apply ()
                      {
                        final f2j.Closure x167 = (f2j.Closure) x166.arg;
                        final f2j.Closure x168 = x167;
                        class Fun169 extends f2j.Closure
                        {
                          f2j.Closure x170 = this;
                          public void apply ()
                          {
                            final java.lang.Integer x171 = (java.lang.Integer) x170.arg;
                            final java.lang.Integer x172 = x171;
                            f2j.Closure x173 = x168;
                            x173.arg = x172;
                            x173.apply();
                            final Object x174 = x173.res;
                            final Object x175 = x174;
                            res = x175;
                          }
                        }
                        f2j.Closure x176 = x164;
                        x176.arg = new Fun169();
                        x176.apply();
                        final Object x177 = x176.res;
                        final Object x178 = x177;
                        res = x178;
                      }
                    }
                    res = new Fun165();
                  }
                }
                f2j.Closure x179 = x103;
                x179.arg = new Fun151();
                x179.apply();
                final Object x180 = x179.res;
                res = x180;
              }
            }
            class Fun181 extends f2j.Closure
            {
              f2j.Closure x182 = this;
              public void apply ()
              {
                final java.lang.Integer x183 = (java.lang.Integer) x182.arg;
                final java.lang.Integer x184 = x183;
                f2j.Closure x185 = new Fun147();
                x185.arg = x184;
                x185.apply();
                final Object x186 = x185.res;
                final Object x187 = x186;
                res = x187;
              }
            }
            class Fun104 extends f2j.Closure
            {
              f2j.Closure x105 = this;
              public void apply ()
              {
                final java.lang.Integer x106 = (java.lang.Integer) x105.arg;
                final f2j.Closure x107 = x102;
                class Fun108 extends f2j.Closure
                {
                  f2j.Closure x109 = this;
                  public void apply ()
                  {
                    final f2j.Closure x110 = (f2j.Closure) x109.arg;
                    final f2j.Closure x111 = x110;
                    class Fun112 extends f2j.Closure
                    {
                      f2j.Closure x113 = this;
                      public void apply ()
                      {
                        final java.lang.Integer x114 = (java.lang.Integer) x113.arg;
                        final java.lang.Integer x115 = x114;
                        f2j.Closure x116 = x111;
                        x116.arg = x115;
                        x116.apply();
                        final Object x117 = x116.res;
                        final Object x118 = x117;
                        res = x118;
                      }
                    }
                    f2j.Closure x119 = x107;
                    x119.arg = new Fun112();
                    x119.apply();
                    final f2j.Closure x120 = (f2j.Closure) x119.res;
                    final f2j.Closure x121 = x120;
                    class Fun122 extends f2j.Closure
                    {
                      f2j.Closure x123 = this;
                      public void apply ()
                      {
                        final f2j.Closure x124 = (f2j.Closure) x123.arg;
                        final f2j.Closure x125 = x124;
                        class Fun126 extends f2j.Closure
                        {
                          f2j.Closure x127 = this;
                          public void apply ()
                          {
                            final java.lang.Integer x128 = (java.lang.Integer) x127.arg;
                            final java.lang.Integer x129 = x128;
                            f2j.Closure x130 = x125;
                            x130.arg = x129;
                            x130.apply();
                            final Object x131 = x130.res;
                            final Object x132 = x131;
                            res = x132;
                          }
                        }
                        f2j.Closure x133 = x121;
                        x133.arg = new Fun126();
                        x133.apply();
                        final Object x134 = x133.res;
                        final Object x135 = x134;
                        res = x135;
                      }
                    }
                    res = new Fun122();
                  }
                }
                f2j.Closure x136 = x103;
                x136.arg = new Fun108();
                x136.apply();
                final Object x137 = x136.res;
                res = x137;
              }
            }
            class Fun138 extends f2j.Closure
            {
              f2j.Closure x139 = this;
              public void apply ()
              {
                final java.lang.Integer x140 = (java.lang.Integer) x139.arg;
                final java.lang.Integer x141 = x140;
                f2j.Closure x142 = new Fun104();
                x142.arg = x141;
                x142.apply();
                final Object x143 = x142.res;
                final Object x144 = x143;
                res = x144;
              }
            }
            f2j.Closure x145 = x102;
            x145.arg = new Fun138();
            x145.apply();
            final f2j.Closure x146 = (f2j.Closure) x145.res;
            f2j.Closure x188 = x146;
            x188.arg = new Fun181();
            x188.apply();
            final Object x189 = x188.res;
            res = x189;
          }
        }
        x9 = new Fun100();
        class Fun190 extends f2j.Closure
        {
          f2j.Closure x191 = this;
          public void apply ()
          {
            final f2j.Closure x192 = (f2j.Closure) x191.arg;
            class Fun193 extends f2j.Closure
            {
              f2j.Closure x194 = this;
              public void apply ()
              {
                final f2j.Closure x195 = (f2j.Closure) x194.arg;
                class Fun196 extends f2j.Closure
                {
                  f2j.Closure x197 = this;
                  public void apply ()
                  {
                    final f2j.Closure x198 = (f2j.Closure) x197.arg;
                    class Fun199 extends f2j.Closure
                    {
                      f2j.Closure x200 = this;
                      public void apply ()
                      {
                        final f2j.Closure x201 = (f2j.Closure) x200.arg;
                        final f2j.Closure x234 = x201;
                        class Fun235 extends f2j.Closure
                        {
                          f2j.Closure x236 = this;
                          public void apply ()
                          {
                            final java.lang.Integer x237 = (java.lang.Integer) x236.arg;
                            final java.lang.Integer x238 = x237;
                            f2j.Closure x239 = x234;
                            x239.arg = x238;
                            x239.apply();
                            final Object x240 = x239.res;
                            final Object x241 = x240;
                            res = x241;
                          }
                        }
                        class Fun202 extends f2j.Closure
                        {
                          f2j.Closure x203 = this;
                          public void apply ()
                          {
                            final java.lang.Integer x204 = (java.lang.Integer) x203.arg;
                            final f2j.Closure x215 = x201;
                            class Fun216 extends f2j.Closure
                            {
                              f2j.Closure x217 = this;
                              public void apply ()
                              {
                                final java.lang.Integer x218 = (java.lang.Integer) x217.arg;
                                final java.lang.Integer x219 = x218;
                                f2j.Closure x220 = x215;
                                x220.arg = x219;
                                x220.apply();
                                final Object x221 = x220.res;
                                final Object x222 = x221;
                                res = x222;
                              }
                            }
                            final f2j.Closure x205 = x198;
                            class Fun206 extends f2j.Closure
                            {
                              f2j.Closure x207 = this;
                              public void apply ()
                              {
                                final java.lang.Integer x208 = (java.lang.Integer) x207.arg;
                                final java.lang.Integer x209 = x208;
                                f2j.Closure x210 = x205;
                                x210.arg = x209;
                                x210.apply();
                                final Object x211 = x210.res;
                                final Object x212 = x211;
                                res = x212;
                              }
                            }
                            f2j.Closure x213 = x195;
                            x213.arg = new Fun206();
                            x213.apply();
                            final f2j.Closure x214 = (f2j.Closure) x213.res;
                            f2j.Closure x223 = x214;
                            x223.arg = new Fun216();
                            x223.apply();
                            final Object x224 = x223.res;
                            res = x224;
                          }
                        }
                        class Fun225 extends f2j.Closure
                        {
                          f2j.Closure x226 = this;
                          public void apply ()
                          {
                            final java.lang.Integer x227 = (java.lang.Integer) x226.arg;
                            final java.lang.Integer x228 = x227;
                            f2j.Closure x229 = new Fun202();
                            x229.arg = x228;
                            x229.apply();
                            final Object x230 = x229.res;
                            final Object x231 = x230;
                            res = x231;
                          }
                        }
                        f2j.Closure x232 = x192;
                        x232.arg = new Fun225();
                        x232.apply();
                        final f2j.Closure x233 = (f2j.Closure) x232.res;
                        f2j.Closure x242 = x233;
                        x242.arg = new Fun235();
                        x242.apply();
                        final Object x243 = x242.res;
                        res = x243;
                      }
                    }
                    res = new Fun199();
                  }
                }
                res = new Fun196();
              }
            }
            res = new Fun193();
          }
        }
        class Fun244 extends f2j.Closure
        {
          f2j.Closure x245 = this;
          public void apply ()
          {
            final f2j.Closure x246 = (f2j.Closure) x245.arg;
            class Fun247 extends f2j.Closure
            {
              f2j.Closure x248 = this;
              public void apply ()
              {
                final f2j.Closure x249 = (f2j.Closure) x248.arg;
                class Fun250 extends f2j.Closure
                {
                  f2j.Closure x251 = this;
                  public void apply ()
                  {
                    final java.lang.Integer x252 = (java.lang.Integer) x251.arg;
                    final java.lang.Boolean x254 = x252 == 0;
                    java.lang.Integer ifres253;
                    if (x254)
                    {
                      ifres253 = 1;
                    }
                    else
                    {
                      final java.lang.Integer x258 = x252 - 1;
                      final java.lang.Integer x259 = x258;
                      final java.lang.Integer x255 = 0;
                      f2j.Closure x256 = x249;
                      x256.arg = x255;
                      x256.apply();
                      final f2j.Closure x257 = (f2j.Closure) x256.res;
                      f2j.Closure x260 = x257;
                      x260.arg = x259;
                      x260.apply();
                      final java.lang.Integer x261 = (java.lang.Integer) x260.res;
                      final java.lang.Integer x262 = x252 * x261;
                      ifres253 = x262;
                    }
                    res = ifres253;
                  }
                }
                res = new Fun250();
              }
            }
            res = new Fun247();
          }
        }
        class Fun263 extends f2j.Closure
        {
          f2j.Closure x264 = this;
          public void apply ()
          {
            final f2j.Closure x265 = (f2j.Closure) x264.arg;
            class Fun266 extends f2j.Closure
            {
              f2j.Closure x267 = this;
              public void apply ()
              {
                final f2j.Closure x268 = (f2j.Closure) x267.arg;
                class Fun269 extends f2j.Closure
                {
                  f2j.Closure x270 = this;
                  public void apply ()
                  {
                    final java.lang.Integer x271 = (java.lang.Integer) x270.arg;
                    final java.io.PrintStream x272 = (java.io.PrintStream) java.lang.System.out;
                    x272.<java.lang.String>println("Hola");
                    final java.lang.Integer x276 = x271;
                    final java.lang.Integer x273 = 0;
                    f2j.Closure x274 = x265;
                    x274.arg = x273;
                    x274.apply();
                    final f2j.Closure x275 = (f2j.Closure) x274.res;
                    f2j.Closure x277 = x275;
                    x277.arg = x276;
                    x277.apply();
                    final java.lang.Integer x278 = (java.lang.Integer) x277.res;
                    res = x278;
                  }
                }
                res = new Fun269();
              }
            }
            res = new Fun266();
          }
        }
        final f2j.tuples.Tuple2 x279 = new f2j.tuples.Tuple2(new Fun244(), new Fun263());
        final f2j.tuples.Tuple2 x280 = x279;
        final java.lang.Integer x439 = 5;
        final f2j.Closure x334 = (f2j.Closure) x280._1;
        final f2j.Closure x335 = x334;
        class Fun336 extends f2j.Closure
        {
          f2j.Closure x337 = this;
          public void apply ()
          {
            final f2j.Closure x338 = (f2j.Closure) x337.arg;
            final f2j.Closure x339 = x338;
            class Fun340 extends f2j.Closure
            {
              f2j.Closure x341 = this;
              public void apply ()
              {
                final java.lang.Integer x342 = (java.lang.Integer) x341.arg;
                final java.lang.Integer x343 = x342;
                f2j.Closure x344 = x339;
                x344.arg = x343;
                x344.apply();
                final f2j.Closure x345 = (f2j.Closure) x344.res;
                final f2j.Closure x346 = x345;
                class Fun347 extends f2j.Closure
                {
                  f2j.Closure x348 = this;
                  public void apply ()
                  {
                    final java.lang.Integer x349 = (java.lang.Integer) x348.arg;
                    final java.lang.Integer x350 = x349;
                    f2j.Closure x351 = x346;
                    x351.arg = x350;
                    x351.apply();
                    final java.lang.Integer x352 = (java.lang.Integer) x351.res;
                    final java.lang.Integer x353 = x352;
                    res = x353;
                  }
                }
                res = new Fun347();
              }
            }
            f2j.Closure x354 = x335;
            x354.arg = new Fun340();
            x354.apply();
            final f2j.Closure x355 = (f2j.Closure) x354.res;
            final f2j.Closure x356 = x355;
            class Fun357 extends f2j.Closure
            {
              f2j.Closure x358 = this;
              public void apply ()
              {
                final f2j.Closure x359 = (f2j.Closure) x358.arg;
                final f2j.Closure x360 = x359;
                class Fun361 extends f2j.Closure
                {
                  f2j.Closure x362 = this;
                  public void apply ()
                  {
                    final java.lang.Integer x363 = (java.lang.Integer) x362.arg;
                    final java.lang.Integer x364 = x363;
                    f2j.Closure x365 = x360;
                    x365.arg = x364;
                    x365.apply();
                    final f2j.Closure x366 = (f2j.Closure) x365.res;
                    final f2j.Closure x367 = x366;
                    class Fun368 extends f2j.Closure
                    {
                      f2j.Closure x369 = this;
                      public void apply ()
                      {
                        final java.lang.Integer x370 = (java.lang.Integer) x369.arg;
                        final java.lang.Integer x371 = x370;
                        f2j.Closure x372 = x367;
                        x372.arg = x371;
                        x372.apply();
                        final java.lang.Integer x373 = (java.lang.Integer) x372.res;
                        final java.lang.Integer x374 = x373;
                        res = x374;
                      }
                    }
                    res = new Fun368();
                  }
                }
                f2j.Closure x375 = x356;
                x375.arg = new Fun361();
                x375.apply();
                final f2j.Closure x376 = (f2j.Closure) x375.res;
                final f2j.Closure x377 = x376;
                class Fun378 extends f2j.Closure
                {
                  f2j.Closure x379 = this;
                  public void apply ()
                  {
                    final java.lang.Integer x380 = (java.lang.Integer) x379.arg;
                    final java.lang.Integer x381 = x380;
                    f2j.Closure x382 = x377;
                    x382.arg = x381;
                    x382.apply();
                    final java.lang.Integer x383 = (java.lang.Integer) x382.res;
                    final java.lang.Integer x384 = x383;
                    res = x384;
                  }
                }
                res = new Fun378();
              }
            }
            res = new Fun357();
          }
        }
        final f2j.Closure x281 = (f2j.Closure) x280._2;
        final f2j.Closure x282 = x281;
        class Fun283 extends f2j.Closure
        {
          f2j.Closure x284 = this;
          public void apply ()
          {
            final f2j.Closure x285 = (f2j.Closure) x284.arg;
            final f2j.Closure x286 = x285;
            class Fun287 extends f2j.Closure
            {
              f2j.Closure x288 = this;
              public void apply ()
              {
                final java.lang.Integer x289 = (java.lang.Integer) x288.arg;
                final java.lang.Integer x290 = x289;
                f2j.Closure x291 = x286;
                x291.arg = x290;
                x291.apply();
                final f2j.Closure x292 = (f2j.Closure) x291.res;
                final f2j.Closure x293 = x292;
                class Fun294 extends f2j.Closure
                {
                  f2j.Closure x295 = this;
                  public void apply ()
                  {
                    final java.lang.Integer x296 = (java.lang.Integer) x295.arg;
                    final java.lang.Integer x297 = x296;
                    f2j.Closure x298 = x293;
                    x298.arg = x297;
                    x298.apply();
                    final java.lang.Integer x299 = (java.lang.Integer) x298.res;
                    final java.lang.Integer x300 = x299;
                    res = x300;
                  }
                }
                res = new Fun294();
              }
            }
            f2j.Closure x301 = x282;
            x301.arg = new Fun287();
            x301.apply();
            final f2j.Closure x302 = (f2j.Closure) x301.res;
            final f2j.Closure x303 = x302;
            class Fun304 extends f2j.Closure
            {
              f2j.Closure x305 = this;
              public void apply ()
              {
                final f2j.Closure x306 = (f2j.Closure) x305.arg;
                final f2j.Closure x307 = x306;
                class Fun308 extends f2j.Closure
                {
                  f2j.Closure x309 = this;
                  public void apply ()
                  {
                    final java.lang.Integer x310 = (java.lang.Integer) x309.arg;
                    final java.lang.Integer x311 = x310;
                    f2j.Closure x312 = x307;
                    x312.arg = x311;
                    x312.apply();
                    final f2j.Closure x313 = (f2j.Closure) x312.res;
                    final f2j.Closure x314 = x313;
                    class Fun315 extends f2j.Closure
                    {
                      f2j.Closure x316 = this;
                      public void apply ()
                      {
                        final java.lang.Integer x317 = (java.lang.Integer) x316.arg;
                        final java.lang.Integer x318 = x317;
                        f2j.Closure x319 = x314;
                        x319.arg = x318;
                        x319.apply();
                        final java.lang.Integer x320 = (java.lang.Integer) x319.res;
                        final java.lang.Integer x321 = x320;
                        res = x321;
                      }
                    }
                    res = new Fun315();
                  }
                }
                f2j.Closure x322 = x303;
                x322.arg = new Fun308();
                x322.apply();
                final f2j.Closure x323 = (f2j.Closure) x322.res;
                final f2j.Closure x324 = x323;
                class Fun325 extends f2j.Closure
                {
                  f2j.Closure x326 = this;
                  public void apply ()
                  {
                    final java.lang.Integer x327 = (java.lang.Integer) x326.arg;
                    final java.lang.Integer x328 = x327;
                    f2j.Closure x329 = x324;
                    x329.arg = x328;
                    x329.apply();
                    final java.lang.Integer x330 = (java.lang.Integer) x329.res;
                    final java.lang.Integer x331 = x330;
                    res = x331;
                  }
                }
                res = new Fun325();
              }
            }
            res = new Fun304();
          }
        }
        f2j.Closure x332 = new Fun190();
        x332.arg = new Fun283();
        x332.apply();
        final f2j.Closure x333 = (f2j.Closure) x332.res;
        f2j.Closure x385 = x333;
        x385.arg = new Fun336();
        x385.apply();
        final f2j.Closure x386 = (f2j.Closure) x385.res;
        final f2j.Closure x387 = x386;
        class Fun388 extends f2j.Closure
        {
          f2j.Closure x389 = this;
          public void apply ()
          {
            final f2j.Closure x390 = (f2j.Closure) x389.arg;
            final f2j.Closure x391 = x390;
            class Fun392 extends f2j.Closure
            {
              f2j.Closure x393 = this;
              public void apply ()
              {
                final java.lang.Integer x394 = (java.lang.Integer) x393.arg;
                final java.lang.Integer x395 = x394;
                f2j.Closure x396 = x391;
                x396.arg = x395;
                x396.apply();
                final f2j.Closure x397 = (f2j.Closure) x396.res;
                final f2j.Closure x398 = x397;
                class Fun399 extends f2j.Closure
                {
                  f2j.Closure x400 = this;
                  public void apply ()
                  {
                    final java.lang.Integer x401 = (java.lang.Integer) x400.arg;
                    final java.lang.Integer x402 = x401;
                    f2j.Closure x403 = x398;
                    x403.arg = x402;
                    x403.apply();
                    final java.lang.Integer x404 = (java.lang.Integer) x403.res;
                    final java.lang.Integer x405 = x404;
                    res = x405;
                  }
                }
                res = new Fun399();
              }
            }
            f2j.Closure x406 = x387;
            x406.arg = new Fun392();
            x406.apply();
            final f2j.Closure x407 = (f2j.Closure) x406.res;
            final f2j.Closure x408 = x407;
            class Fun409 extends f2j.Closure
            {
              f2j.Closure x410 = this;
              public void apply ()
              {
                final f2j.Closure x411 = (f2j.Closure) x410.arg;
                final f2j.Closure x412 = x411;
                class Fun413 extends f2j.Closure
                {
                  f2j.Closure x414 = this;
                  public void apply ()
                  {
                    final java.lang.Integer x415 = (java.lang.Integer) x414.arg;
                    final java.lang.Integer x416 = x415;
                    f2j.Closure x417 = x412;
                    x417.arg = x416;
                    x417.apply();
                    final f2j.Closure x418 = (f2j.Closure) x417.res;
                    final f2j.Closure x419 = x418;
                    class Fun420 extends f2j.Closure
                    {
                      f2j.Closure x421 = this;
                      public void apply ()
                      {
                        final java.lang.Integer x422 = (java.lang.Integer) x421.arg;
                        final java.lang.Integer x423 = x422;
                        f2j.Closure x424 = x419;
                        x424.arg = x423;
                        x424.apply();
                        final java.lang.Integer x425 = (java.lang.Integer) x424.res;
                        final java.lang.Integer x426 = x425;
                        res = x426;
                      }
                    }
                    res = new Fun420();
                  }
                }
                f2j.Closure x427 = x408;
                x427.arg = new Fun413();
                x427.apply();
                final f2j.Closure x428 = (f2j.Closure) x427.res;
                final f2j.Closure x429 = x428;
                class Fun430 extends f2j.Closure
                {
                  f2j.Closure x431 = this;
                  public void apply ()
                  {
                    final java.lang.Integer x432 = (java.lang.Integer) x431.arg;
                    final java.lang.Integer x433 = x432;
                    f2j.Closure x434 = x429;
                    x434.arg = x433;
                    x434.apply();
                    final java.lang.Integer x435 = (java.lang.Integer) x434.res;
                    final java.lang.Integer x436 = x435;
                    res = x436;
                  }
                }
                res = new Fun430();
              }
            }
            res = new Fun409();
          }
        }
        f2j.Closure x437 = x9;
        x437.arg = new Fun388();
        x437.apply();
        final f2j.Closure x438 = (f2j.Closure) x437.res;
        f2j.Closure x440 = x438;
        x440.arg = x439;
        x440.apply();
        final java.lang.Integer x441 = (java.lang.Integer) x440.res;
        temp = x441;
      }
    }
    Let7 x7 = new Let7();
    final java.lang.Integer x8 = (java.lang.Integer) x7.temp;
    return x8;
  }
  public static void main (String[] args)
  {
    System.out.println(apply());
  }
}