public class ObjectAlgebra
{
  static Object apply ()
  {
    class Fun1 extends f2j.Closure
    {
      f2j.Closure x2 = this;
      public void apply ()
      {
        final java.lang.Integer x3 = (java.lang.Integer) x2.arg;
        res = x3;
      }
    }
    class Fun4 extends f2j.Closure
    {
      f2j.Closure x5 = this;
      public void apply ()
      {
        final java.lang.Integer x6 = (java.lang.Integer) x5.arg;
        class Fun7 extends f2j.Closure
        {
          f2j.Closure x8 = this;
          public void apply ()
          {
            final java.lang.Integer x9 = (java.lang.Integer) x8.arg;
            final java.lang.Integer x10 = x6 + x9;
            res = x10;
          }
        }
        res = new Fun7();
      }
    }
    final f2j.tuples.Tuple2 x11 = new f2j.tuples.Tuple2(new Fun1(), new Fun4());
    final f2j.tuples.Tuple2 x12 = x11;
    class Fun13 extends f2j.Closure
    {
      f2j.Closure x14 = this;
      public void apply ()
      {
        final java.lang.Integer x15 = (java.lang.Integer) x14.arg;
        class Fun16 extends f2j.Closure
        {
          f2j.Closure x17 = this;
          public void apply ()
          {
            final java.lang.Integer x18 = (java.lang.Integer) x17.arg;
            final java.lang.Integer x19 = x15 - x18;
            res = x19;
          }
        }
        res = new Fun16();
      }
    }
    final f2j.tuples.Tuple2 x20 = new f2j.tuples.Tuple2(x12, new Fun13());
    final f2j.tuples.Tuple2 x21 = x20;
    class Fun22 extends f2j.Closure
    {
      f2j.Closure x23 = this;
      public void apply ()
      {
        final java.lang.Integer x24 = (java.lang.Integer) x23.arg;
        final java.lang.String x25 = x24.toString();
        res = x25;
      }
    }
    class Fun26 extends f2j.Closure
    {
      f2j.Closure x27 = this;
      public void apply ()
      {
        final java.lang.String x28 = (java.lang.String) x27.arg;
        class Fun29 extends f2j.Closure
        {
          f2j.Closure x30 = this;
          public void apply ()
          {
            final java.lang.String x31 = (java.lang.String) x30.arg;
            final java.lang.String x32 = x31.toString();
            final java.lang.String x33 = x28.toString();
            final java.lang.String x34 = x33.<java.lang.String>concat(" + ");
            final java.lang.String x35 = x34.<java.lang.String>concat(x32);
            res = x35;
          }
        }
        res = new Fun29();
      }
    }
    final f2j.tuples.Tuple2 x36 = new f2j.tuples.Tuple2(new Fun22(), new Fun26());
    class Fun37 extends f2j.Closure
    {
      f2j.Closure x38 = this;
      public void apply ()
      {
        final java.lang.String x39 = (java.lang.String) x38.arg;
        class Fun40 extends f2j.Closure
        {
          f2j.Closure x41 = this;
          public void apply ()
          {
            final java.lang.String x42 = (java.lang.String) x41.arg;
            final java.lang.String x43 = x42.toString();
            final java.lang.String x44 = x39.toString();
            final java.lang.String x45 = x44.<java.lang.String>concat(" - ");
            final java.lang.String x46 = x45.<java.lang.String>concat(x43);
            res = x46;
          }
        }
        res = new Fun40();
      }
    }
    final f2j.tuples.Tuple2 x47 = new f2j.tuples.Tuple2(x36, new Fun37());
    final f2j.tuples.Tuple2 x48 = x47;
    class Fun49 extends f2j.Closure
    {
      f2j.Closure x50 = this;
      public void apply ()
      {
        final f2j.tuples.Tuple2 x51 = (f2j.tuples.Tuple2) x50.arg;
        final f2j.tuples.Tuple2 x52 = x51;
        final f2j.Closure x53 = (f2j.Closure) x52._2;
        final f2j.Closure x54 = x53;
        final f2j.tuples.Tuple2 x64 = x51;
        final f2j.Closure x65 = (f2j.Closure) x64._1;
        final f2j.Closure x66 = x65;
        final java.lang.Integer x67 = 6;
        f2j.Closure x68 = x66;
        x68.arg = x67;
        x68.apply();
        final Object x69 = x68.res;
        final Object x70 = x69;
        final f2j.tuples.Tuple2 x55 = x51;
        final f2j.Closure x56 = (f2j.Closure) x55._1;
        final f2j.Closure x57 = x56;
        final java.lang.Integer x58 = 6;
        f2j.Closure x59 = x57;
        x59.arg = x58;
        x59.apply();
        final Object x60 = x59.res;
        final Object x61 = x60;
        f2j.Closure x62 = x54;
        x62.arg = x61;
        x62.apply();
        final f2j.Closure x63 = (f2j.Closure) x62.res;
        f2j.Closure x71 = x63;
        x71.arg = x70;
        x71.apply();
        final Object x72 = x71.res;
        res = x72;
      }
    }
    class Fun73 extends f2j.Closure
    {
      f2j.Closure x74 = this;
      public void apply ()
      {
        final f2j.tuples.Tuple2 x75 = (f2j.tuples.Tuple2) x74.arg;
        final f2j.tuples.Tuple2 x76 = x75;
        final f2j.Closure x77 = (f2j.Closure) x76._2;
        final f2j.Closure x78 = x77;
        final f2j.tuples.Tuple2 x117 = x75;
        final f2j.tuples.Tuple2 x118 = (f2j.tuples.Tuple2) x117._1;
        final f2j.tuples.Tuple2 x119 = x118;
        final f2j.Closure x120 = (f2j.Closure) x119._1;
        final f2j.Closure x121 = x120;
        final java.lang.Integer x122 = 2;
        f2j.Closure x123 = x121;
        x123.arg = x122;
        x123.apply();
        final Object x124 = x123.res;
        final Object x125 = x124;
        final f2j.tuples.Tuple2 x79 = x75;
        final f2j.tuples.Tuple2 x80 = x79;
        final f2j.tuples.Tuple2 x81 = (f2j.tuples.Tuple2) x80._1;
        final f2j.tuples.Tuple2 x82 = x81;
        final f2j.Closure x83 = (f2j.Closure) x82._1;
        final f2j.Closure x84 = x83;
        class Fun85 extends f2j.Closure
        {
          f2j.Closure x86 = this;
          public void apply ()
          {
            final java.lang.Integer x87 = (java.lang.Integer) x86.arg;
            final java.lang.Integer x88 = x87;
            f2j.Closure x89 = x84;
            x89.arg = x88;
            x89.apply();
            final Object x90 = x89.res;
            final Object x91 = x90;
            res = x91;
          }
        }
        final f2j.tuples.Tuple2 x92 = x79;
        final f2j.tuples.Tuple2 x93 = (f2j.tuples.Tuple2) x92._1;
        final f2j.tuples.Tuple2 x94 = x93;
        final f2j.Closure x95 = (f2j.Closure) x94._2;
        final f2j.Closure x96 = x95;
        class Fun97 extends f2j.Closure
        {
          f2j.Closure x98 = this;
          public void apply ()
          {
            final Object x99 = x98.arg;
            final Object x100 = x99;
            f2j.Closure x101 = x96;
            x101.arg = x100;
            x101.apply();
            final f2j.Closure x102 = (f2j.Closure) x101.res;
            final f2j.Closure x103 = x102;
            class Fun104 extends f2j.Closure
            {
              f2j.Closure x105 = this;
              public void apply ()
              {
                final Object x106 = x105.arg;
                final Object x107 = x106;
                f2j.Closure x108 = x103;
                x108.arg = x107;
                x108.apply();
                final Object x109 = x108.res;
                final Object x110 = x109;
                res = x110;
              }
            }
            res = new Fun104();
          }
        }
        final f2j.tuples.Tuple2 x111 = new f2j.tuples.Tuple2(new Fun85(), new Fun97());
        f2j.Closure x112 = new Fun49();
        x112.arg = x111;
        x112.apply();
        final Object x113 = x112.res;
        final Object x114 = x113;
        f2j.Closure x115 = x78;
        x115.arg = x114;
        x115.apply();
        final f2j.Closure x116 = (f2j.Closure) x115.res;
        f2j.Closure x126 = x116;
        x126.arg = x125;
        x126.apply();
        final Object x127 = x126.res;
        res = x127;
      }
    }
    class Fun128 extends f2j.Closure
    {
      f2j.Closure x129 = this;
      public void apply ()
      {
        final f2j.tuples.Tuple2 x130 = (f2j.tuples.Tuple2) x129.arg;
        class Fun131 extends f2j.Closure
        {
          f2j.Closure x132 = this;
          public void apply ()
          {
            final f2j.tuples.Tuple2 x133 = (f2j.tuples.Tuple2) x132.arg;
            class Fun134 extends f2j.Closure
            {
              f2j.Closure x135 = this;
              public void apply ()
              {
                final java.lang.Integer x136 = (java.lang.Integer) x135.arg;
                final f2j.tuples.Tuple2 x137 = x130;
                final f2j.Closure x138 = (f2j.Closure) x137._1;
                final f2j.Closure x139 = x138;
                final java.lang.Integer x140 = x136;
                f2j.Closure x141 = x139;
                x141.arg = x140;
                x141.apply();
                final Object x142 = x141.res;
                final f2j.tuples.Tuple2 x143 = x133;
                final f2j.Closure x144 = (f2j.Closure) x143._1;
                final f2j.Closure x145 = x144;
                final java.lang.Integer x146 = x136;
                f2j.Closure x147 = x145;
                x147.arg = x146;
                x147.apply();
                final Object x148 = x147.res;
                final f2j.tuples.Tuple2 x149 = new f2j.tuples.Tuple2(x142, x148);
                res = x149;
              }
            }
            class Fun150 extends f2j.Closure
            {
              f2j.Closure x151 = this;
              public void apply ()
              {
                final f2j.tuples.Tuple2 x152 = (f2j.tuples.Tuple2) x151.arg;
                class Fun153 extends f2j.Closure
                {
                  f2j.Closure x154 = this;
                  public void apply ()
                  {
                    final f2j.tuples.Tuple2 x155 = (f2j.tuples.Tuple2) x154.arg;
                    final f2j.tuples.Tuple2 x156 = x130;
                    final f2j.Closure x157 = (f2j.Closure) x156._2;
                    final f2j.Closure x158 = x157;
                    final f2j.tuples.Tuple2 x164 = x155;
                    final Object x165 = (Object) x164._1;
                    final Object x166 = x165;
                    final f2j.tuples.Tuple2 x159 = x152;
                    final Object x160 = (Object) x159._1;
                    final Object x161 = x160;
                    f2j.Closure x162 = x158;
                    x162.arg = x161;
                    x162.apply();
                    final f2j.Closure x163 = (f2j.Closure) x162.res;
                    f2j.Closure x167 = x163;
                    x167.arg = x166;
                    x167.apply();
                    final Object x168 = x167.res;
                    final f2j.tuples.Tuple2 x169 = x133;
                    final f2j.Closure x170 = (f2j.Closure) x169._2;
                    final f2j.Closure x171 = x170;
                    final f2j.tuples.Tuple2 x177 = x155;
                    final Object x178 = (Object) x177._2;
                    final Object x179 = x178;
                    final f2j.tuples.Tuple2 x172 = x152;
                    final Object x173 = (Object) x172._2;
                    final Object x174 = x173;
                    f2j.Closure x175 = x171;
                    x175.arg = x174;
                    x175.apply();
                    final f2j.Closure x176 = (f2j.Closure) x175.res;
                    f2j.Closure x180 = x176;
                    x180.arg = x179;
                    x180.apply();
                    final Object x181 = x180.res;
                    final f2j.tuples.Tuple2 x182 = new f2j.tuples.Tuple2(x168, x181);
                    res = x182;
                  }
                }
                res = new Fun153();
              }
            }
            final f2j.tuples.Tuple2 x183 = new f2j.tuples.Tuple2(new Fun134(), new Fun150());
            res = x183;
          }
        }
        res = new Fun131();
      }
    }
    final f2j.tuples.Tuple2 x219 = x48;
    final f2j.tuples.Tuple2 x220 = x219;
    final f2j.tuples.Tuple2 x221 = (f2j.tuples.Tuple2) x220._1;
    final f2j.tuples.Tuple2 x222 = x221;
    final f2j.Closure x223 = (f2j.Closure) x222._1;
    final f2j.Closure x224 = x223;
    class Fun225 extends f2j.Closure
    {
      f2j.Closure x226 = this;
      public void apply ()
      {
        final java.lang.Integer x227 = (java.lang.Integer) x226.arg;
        final java.lang.Integer x228 = x227;
        f2j.Closure x229 = x224;
        x229.arg = x228;
        x229.apply();
        final java.lang.String x230 = (java.lang.String) x229.res;
        final java.lang.String x231 = x230;
        res = x231;
      }
    }
    final f2j.tuples.Tuple2 x232 = x219;
    final f2j.tuples.Tuple2 x233 = (f2j.tuples.Tuple2) x232._1;
    final f2j.tuples.Tuple2 x234 = x233;
    final f2j.Closure x235 = (f2j.Closure) x234._2;
    final f2j.Closure x236 = x235;
    class Fun237 extends f2j.Closure
    {
      f2j.Closure x238 = this;
      public void apply ()
      {
        final java.lang.String x239 = (java.lang.String) x238.arg;
        final java.lang.String x240 = x239;
        f2j.Closure x241 = x236;
        x241.arg = x240;
        x241.apply();
        final f2j.Closure x242 = (f2j.Closure) x241.res;
        final f2j.Closure x243 = x242;
        class Fun244 extends f2j.Closure
        {
          f2j.Closure x245 = this;
          public void apply ()
          {
            final java.lang.String x246 = (java.lang.String) x245.arg;
            final java.lang.String x247 = x246;
            f2j.Closure x248 = x243;
            x248.arg = x247;
            x248.apply();
            final java.lang.String x249 = (java.lang.String) x248.res;
            final java.lang.String x250 = x249;
            res = x250;
          }
        }
        res = new Fun244();
      }
    }
    final f2j.tuples.Tuple2 x251 = new f2j.tuples.Tuple2(new Fun225(), new Fun237());
    final f2j.tuples.Tuple2 x184 = x21;
    final f2j.tuples.Tuple2 x185 = x184;
    final f2j.tuples.Tuple2 x186 = (f2j.tuples.Tuple2) x185._1;
    final f2j.tuples.Tuple2 x187 = x186;
    final f2j.Closure x188 = (f2j.Closure) x187._1;
    final f2j.Closure x189 = x188;
    class Fun190 extends f2j.Closure
    {
      f2j.Closure x191 = this;
      public void apply ()
      {
        final java.lang.Integer x192 = (java.lang.Integer) x191.arg;
        final java.lang.Integer x193 = x192;
        f2j.Closure x194 = x189;
        x194.arg = x193;
        x194.apply();
        final java.lang.Integer x195 = (java.lang.Integer) x194.res;
        final java.lang.Integer x196 = x195;
        res = x196;
      }
    }
    final f2j.tuples.Tuple2 x197 = x184;
    final f2j.tuples.Tuple2 x198 = (f2j.tuples.Tuple2) x197._1;
    final f2j.tuples.Tuple2 x199 = x198;
    final f2j.Closure x200 = (f2j.Closure) x199._2;
    final f2j.Closure x201 = x200;
    class Fun202 extends f2j.Closure
    {
      f2j.Closure x203 = this;
      public void apply ()
      {
        final java.lang.Integer x204 = (java.lang.Integer) x203.arg;
        final java.lang.Integer x205 = x204;
        f2j.Closure x206 = x201;
        x206.arg = x205;
        x206.apply();
        final f2j.Closure x207 = (f2j.Closure) x206.res;
        final f2j.Closure x208 = x207;
        class Fun209 extends f2j.Closure
        {
          f2j.Closure x210 = this;
          public void apply ()
          {
            final java.lang.Integer x211 = (java.lang.Integer) x210.arg;
            final java.lang.Integer x212 = x211;
            f2j.Closure x213 = x208;
            x213.arg = x212;
            x213.apply();
            final java.lang.Integer x214 = (java.lang.Integer) x213.res;
            final java.lang.Integer x215 = x214;
            res = x215;
          }
        }
        res = new Fun209();
      }
    }
    final f2j.tuples.Tuple2 x216 = new f2j.tuples.Tuple2(new Fun190(), new Fun202());
    f2j.Closure x217 = new Fun128();
    x217.arg = x216;
    x217.apply();
    final f2j.Closure x218 = (f2j.Closure) x217.res;
    f2j.Closure x252 = x218;
    x252.arg = x251;
    x252.apply();
    final f2j.tuples.Tuple2 x253 = (f2j.tuples.Tuple2) x252.res;
    final f2j.tuples.Tuple2 x254 = x253;
    final f2j.tuples.Tuple2 x255 = x254;
    final f2j.tuples.Tuple2 x256 = x255;
    final f2j.Closure x257 = (f2j.Closure) x256._1;
    final f2j.Closure x258 = x257;
    class Fun259 extends f2j.Closure
    {
      f2j.Closure x260 = this;
      public void apply ()
      {
        final java.lang.Integer x261 = (java.lang.Integer) x260.arg;
        final java.lang.Integer x262 = x261;
        f2j.Closure x263 = x258;
        x263.arg = x262;
        x263.apply();
        final f2j.tuples.Tuple2 x264 = (f2j.tuples.Tuple2) x263.res;
        final f2j.tuples.Tuple2 x265 = x264;
        final f2j.tuples.Tuple2 x266 = x265;
        final Object x267 = (Object) x266._1;
        final Object x268 = x267;
        final f2j.tuples.Tuple2 x269 = x265;
        final Object x270 = (Object) x269._2;
        final Object x271 = x270;
        final f2j.tuples.Tuple2 x272 = new f2j.tuples.Tuple2(x268, x271);
        res = x272;
      }
    }
    final f2j.tuples.Tuple2 x273 = x255;
    final f2j.Closure x274 = (f2j.Closure) x273._2;
    final f2j.Closure x275 = x274;
    class Fun276 extends f2j.Closure
    {
      f2j.Closure x277 = this;
      public void apply ()
      {
        final f2j.tuples.Tuple2 x278 = (f2j.tuples.Tuple2) x277.arg;
        final f2j.tuples.Tuple2 x279 = x278;
        final f2j.tuples.Tuple2 x280 = x279;
        final java.lang.Integer x281 = (java.lang.Integer) x280._1;
        final java.lang.Integer x282 = x281;
        final f2j.tuples.Tuple2 x283 = x279;
        final java.lang.String x284 = (java.lang.String) x283._2;
        final java.lang.String x285 = x284;
        final f2j.tuples.Tuple2 x286 = new f2j.tuples.Tuple2(x282, x285);
        f2j.Closure x287 = x275;
        x287.arg = x286;
        x287.apply();
        final f2j.Closure x288 = (f2j.Closure) x287.res;
        final f2j.Closure x289 = x288;
        class Fun290 extends f2j.Closure
        {
          f2j.Closure x291 = this;
          public void apply ()
          {
            final f2j.tuples.Tuple2 x292 = (f2j.tuples.Tuple2) x291.arg;
            final f2j.tuples.Tuple2 x293 = x292;
            final f2j.tuples.Tuple2 x294 = x293;
            final java.lang.Integer x295 = (java.lang.Integer) x294._1;
            final java.lang.Integer x296 = x295;
            final f2j.tuples.Tuple2 x297 = x293;
            final java.lang.String x298 = (java.lang.String) x297._2;
            final java.lang.String x299 = x298;
            final f2j.tuples.Tuple2 x300 = new f2j.tuples.Tuple2(x296, x299);
            f2j.Closure x301 = x289;
            x301.arg = x300;
            x301.apply();
            final f2j.tuples.Tuple2 x302 = (f2j.tuples.Tuple2) x301.res;
            final f2j.tuples.Tuple2 x303 = x302;
            final f2j.tuples.Tuple2 x304 = x303;
            final Object x305 = (Object) x304._1;
            final Object x306 = x305;
            final f2j.tuples.Tuple2 x307 = x303;
            final Object x308 = (Object) x307._2;
            final Object x309 = x308;
            final f2j.tuples.Tuple2 x310 = new f2j.tuples.Tuple2(x306, x309);
            res = x310;
          }
        }
        res = new Fun290();
      }
    }
    final f2j.tuples.Tuple2 x311 = new f2j.tuples.Tuple2(new Fun259(), new Fun276());
    f2j.Closure x312 = new Fun49();
    x312.arg = x311;
    x312.apply();
    final f2j.tuples.Tuple2 x313 = (f2j.tuples.Tuple2) x312.res;
    final f2j.tuples.Tuple2 x314 = x313;
    final java.lang.String x315 = (java.lang.String) x314._2;
    final java.lang.String x316 = x315;
    final java.lang.String x317 = x316;
    return x317;
  }
  public static void main (String[] args)
  {
    System.out.println(apply());
  }
}