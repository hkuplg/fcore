public class ObjectAlgebra3
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
            final java.lang.Integer x10 = x6;
            final java.lang.Integer x11 = x9;
            final java.lang.Integer x12 = x10 + x11;
            res = x12;
          }
        }
        res = new Fun7();
      }
    }
    final f2j.tuples.Tuple2 x13 = new f2j.tuples.Tuple2(new Fun1(), new Fun4());
    final f2j.tuples.Tuple2 x14 = x13;
    class Fun15 extends f2j.Closure
    {
      f2j.Closure x16 = this;
      public void apply ()
      {
        final java.lang.Integer x17 = (java.lang.Integer) x16.arg;
        class Fun18 extends f2j.Closure
        {
          f2j.Closure x19 = this;
          public void apply ()
          {
            final java.lang.Integer x20 = (java.lang.Integer) x19.arg;
            final java.lang.Integer x21 = x17;
            final java.lang.Integer x22 = x20;
            final java.lang.Integer x23 = x21 - x22;
            res = x23;
          }
        }
        res = new Fun18();
      }
    }
    final f2j.tuples.Tuple2 x24 = new f2j.tuples.Tuple2(x14, new Fun15());
    final f2j.tuples.Tuple2 x25 = x24;
    class Fun26 extends f2j.Closure
    {
      f2j.Closure x27 = this;
      public void apply ()
      {
        final java.lang.Integer x28 = (java.lang.Integer) x27.arg;
        final java.lang.String x29 = x28.toString();
        res = x29;
      }
    }
    class Fun30 extends f2j.Closure
    {
      f2j.Closure x31 = this;
      public void apply ()
      {
        final java.lang.String x32 = (java.lang.String) x31.arg;
        class Fun33 extends f2j.Closure
        {
          f2j.Closure x34 = this;
          public void apply ()
          {
            final java.lang.String x35 = (java.lang.String) x34.arg;
            final java.lang.String x36 = x35;
            final java.lang.String x37 = x32;
            final java.lang.String x38 = x37.<java.lang.String>concat(" + ");
            final java.lang.String x39 = x38.<java.lang.String>concat(x36);
            res = x39;
          }
        }
        res = new Fun33();
      }
    }
    final f2j.tuples.Tuple2 x40 = new f2j.tuples.Tuple2(new Fun26(), new Fun30());
    class Fun41 extends f2j.Closure
    {
      f2j.Closure x42 = this;
      public void apply ()
      {
        final java.lang.String x43 = (java.lang.String) x42.arg;
        class Fun44 extends f2j.Closure
        {
          f2j.Closure x45 = this;
          public void apply ()
          {
            final java.lang.String x46 = (java.lang.String) x45.arg;
            final java.lang.String x47 = x46;
            final java.lang.String x48 = x43;
            final java.lang.String x49 = x48.<java.lang.String>concat(" - ");
            final java.lang.String x50 = x49.<java.lang.String>concat(x47);
            res = x50;
          }
        }
        res = new Fun44();
      }
    }
    final f2j.tuples.Tuple2 x51 = new f2j.tuples.Tuple2(x40, new Fun41());
    final f2j.tuples.Tuple2 x52 = x51;
    class Fun53 extends f2j.Closure
    {
      f2j.Closure x54 = this;
      public void apply ()
      {
        final f2j.tuples.Tuple2 x55 = (f2j.tuples.Tuple2) x54.arg;
        class Fun56 extends f2j.Closure
        {
          f2j.Closure x57 = this;
          public void apply ()
          {
            final f2j.tuples.Tuple2 x58 = (f2j.tuples.Tuple2) x57.arg;
            class Fun59 extends f2j.Closure
            {
              f2j.Closure x60 = this;
              public void apply ()
              {
                final java.lang.Integer x61 = (java.lang.Integer) x60.arg;
                final f2j.tuples.Tuple2 x62 = x55;
                final f2j.Closure x63 = (f2j.Closure) x62._1;
                final f2j.Closure x64 = x63;
                final java.lang.Integer x65 = x61;
                f2j.Closure x66 = x64;
                x66.arg = x65;
                x66.apply();
                final Object x67 = x66.res;
                final f2j.tuples.Tuple2 x68 = x58;
                final f2j.Closure x69 = (f2j.Closure) x68._1;
                final f2j.Closure x70 = x69;
                final java.lang.Integer x71 = x61;
                f2j.Closure x72 = x70;
                x72.arg = x71;
                x72.apply();
                final Object x73 = x72.res;
                final f2j.tuples.Tuple2 x74 = new f2j.tuples.Tuple2(x67, x73);
                res = x74;
              }
            }
            class Fun75 extends f2j.Closure
            {
              f2j.Closure x76 = this;
              public void apply ()
              {
                final f2j.tuples.Tuple2 x77 = (f2j.tuples.Tuple2) x76.arg;
                class Fun78 extends f2j.Closure
                {
                  f2j.Closure x79 = this;
                  public void apply ()
                  {
                    final f2j.tuples.Tuple2 x80 = (f2j.tuples.Tuple2) x79.arg;
                    final f2j.tuples.Tuple2 x81 = x55;
                    final f2j.Closure x82 = (f2j.Closure) x81._2;
                    final f2j.Closure x83 = x82;
                    final f2j.tuples.Tuple2 x89 = x80;
                    final Object x90 = (Object) x89._1;
                    final Object x91 = x90;
                    final f2j.tuples.Tuple2 x84 = x77;
                    final Object x85 = (Object) x84._1;
                    final Object x86 = x85;
                    f2j.Closure x87 = x83;
                    x87.arg = x86;
                    x87.apply();
                    final f2j.Closure x88 = (f2j.Closure) x87.res;
                    f2j.Closure x92 = x88;
                    x92.arg = x91;
                    x92.apply();
                    final Object x93 = x92.res;
                    final f2j.tuples.Tuple2 x94 = x58;
                    final f2j.Closure x95 = (f2j.Closure) x94._2;
                    final f2j.Closure x96 = x95;
                    final f2j.tuples.Tuple2 x102 = x80;
                    final Object x103 = (Object) x102._2;
                    final Object x104 = x103;
                    final f2j.tuples.Tuple2 x97 = x77;
                    final Object x98 = (Object) x97._2;
                    final Object x99 = x98;
                    f2j.Closure x100 = x96;
                    x100.arg = x99;
                    x100.apply();
                    final f2j.Closure x101 = (f2j.Closure) x100.res;
                    f2j.Closure x105 = x101;
                    x105.arg = x104;
                    x105.apply();
                    final Object x106 = x105.res;
                    final f2j.tuples.Tuple2 x107 = new f2j.tuples.Tuple2(x93, x106);
                    res = x107;
                  }
                }
                res = new Fun78();
              }
            }
            final f2j.tuples.Tuple2 x108 = new f2j.tuples.Tuple2(new Fun59(), new Fun75());
            res = x108;
          }
        }
        res = new Fun56();
      }
    }
    class Fun109 extends f2j.Closure
    {
      f2j.Closure x110 = this;
      public void apply ()
      {
        final f2j.tuples.Tuple2 x111 = (f2j.tuples.Tuple2) x110.arg;
        final f2j.tuples.Tuple2 x112 = x111;
        final f2j.Closure x113 = (f2j.Closure) x112._2;
        final f2j.Closure x114 = x113;
        final f2j.tuples.Tuple2 x124 = x111;
        final f2j.Closure x125 = (f2j.Closure) x124._1;
        final f2j.Closure x126 = x125;
        final java.lang.Integer x127 = 6;
        f2j.Closure x128 = x126;
        x128.arg = x127;
        x128.apply();
        final Object x129 = x128.res;
        final Object x130 = x129;
        final f2j.tuples.Tuple2 x115 = x111;
        final f2j.Closure x116 = (f2j.Closure) x115._1;
        final f2j.Closure x117 = x116;
        final java.lang.Integer x118 = 6;
        f2j.Closure x119 = x117;
        x119.arg = x118;
        x119.apply();
        final Object x120 = x119.res;
        final Object x121 = x120;
        f2j.Closure x122 = x114;
        x122.arg = x121;
        x122.apply();
        final f2j.Closure x123 = (f2j.Closure) x122.res;
        f2j.Closure x131 = x123;
        x131.arg = x130;
        x131.apply();
        final Object x132 = x131.res;
        res = x132;
      }
    }
    class Fun133 extends f2j.Closure
    {
      f2j.Closure x134 = this;
      public void apply ()
      {
        final f2j.tuples.Tuple2 x135 = (f2j.tuples.Tuple2) x134.arg;
        final f2j.tuples.Tuple2 x136 = x135;
        final f2j.Closure x137 = (f2j.Closure) x136._2;
        final f2j.Closure x138 = x137;
        final f2j.tuples.Tuple2 x177 = x135;
        final f2j.tuples.Tuple2 x178 = (f2j.tuples.Tuple2) x177._1;
        final f2j.tuples.Tuple2 x179 = x178;
        final f2j.Closure x180 = (f2j.Closure) x179._1;
        final f2j.Closure x181 = x180;
        final java.lang.Integer x182 = 2;
        f2j.Closure x183 = x181;
        x183.arg = x182;
        x183.apply();
        final Object x184 = x183.res;
        final Object x185 = x184;
        final f2j.tuples.Tuple2 x139 = x135;
        final f2j.tuples.Tuple2 x140 = x139;
        final f2j.tuples.Tuple2 x141 = (f2j.tuples.Tuple2) x140._1;
        final f2j.tuples.Tuple2 x142 = x141;
        final f2j.Closure x143 = (f2j.Closure) x142._1;
        final f2j.Closure x144 = x143;
        class Fun145 extends f2j.Closure
        {
          f2j.Closure x146 = this;
          public void apply ()
          {
            final java.lang.Integer x147 = (java.lang.Integer) x146.arg;
            final java.lang.Integer x148 = x147;
            f2j.Closure x149 = x144;
            x149.arg = x148;
            x149.apply();
            final Object x150 = x149.res;
            final Object x151 = x150;
            res = x151;
          }
        }
        final f2j.tuples.Tuple2 x152 = x139;
        final f2j.tuples.Tuple2 x153 = (f2j.tuples.Tuple2) x152._1;
        final f2j.tuples.Tuple2 x154 = x153;
        final f2j.Closure x155 = (f2j.Closure) x154._2;
        final f2j.Closure x156 = x155;
        class Fun157 extends f2j.Closure
        {
          f2j.Closure x158 = this;
          public void apply ()
          {
            final Object x159 = x158.arg;
            final Object x160 = x159;
            f2j.Closure x161 = x156;
            x161.arg = x160;
            x161.apply();
            final f2j.Closure x162 = (f2j.Closure) x161.res;
            final f2j.Closure x163 = x162;
            class Fun164 extends f2j.Closure
            {
              f2j.Closure x165 = this;
              public void apply ()
              {
                final Object x166 = x165.arg;
                final Object x167 = x166;
                f2j.Closure x168 = x163;
                x168.arg = x167;
                x168.apply();
                final Object x169 = x168.res;
                final Object x170 = x169;
                res = x170;
              }
            }
            res = new Fun164();
          }
        }
        final f2j.tuples.Tuple2 x171 = new f2j.tuples.Tuple2(new Fun145(), new Fun157());
        f2j.Closure x172 = new Fun109();
        x172.arg = x171;
        x172.apply();
        final Object x173 = x172.res;
        final Object x174 = x173;
        f2j.Closure x175 = x138;
        x175.arg = x174;
        x175.apply();
        final f2j.Closure x176 = (f2j.Closure) x175.res;
        f2j.Closure x186 = x176;
        x186.arg = x185;
        x186.apply();
        final Object x187 = x186.res;
        res = x187;
      }
    }
    final f2j.tuples.Tuple2 x223 = x52;
    final f2j.tuples.Tuple2 x224 = x223;
    final f2j.tuples.Tuple2 x225 = (f2j.tuples.Tuple2) x224._1;
    final f2j.tuples.Tuple2 x226 = x225;
    final f2j.Closure x227 = (f2j.Closure) x226._1;
    final f2j.Closure x228 = x227;
    class Fun229 extends f2j.Closure
    {
      f2j.Closure x230 = this;
      public void apply ()
      {
        final java.lang.Integer x231 = (java.lang.Integer) x230.arg;
        final java.lang.Integer x232 = x231;
        f2j.Closure x233 = x228;
        x233.arg = x232;
        x233.apply();
        final java.lang.String x234 = (java.lang.String) x233.res;
        final java.lang.String x235 = x234;
        res = x235;
      }
    }
    final f2j.tuples.Tuple2 x236 = x223;
    final f2j.tuples.Tuple2 x237 = (f2j.tuples.Tuple2) x236._1;
    final f2j.tuples.Tuple2 x238 = x237;
    final f2j.Closure x239 = (f2j.Closure) x238._2;
    final f2j.Closure x240 = x239;
    class Fun241 extends f2j.Closure
    {
      f2j.Closure x242 = this;
      public void apply ()
      {
        final java.lang.String x243 = (java.lang.String) x242.arg;
        final java.lang.String x244 = x243;
        f2j.Closure x245 = x240;
        x245.arg = x244;
        x245.apply();
        final f2j.Closure x246 = (f2j.Closure) x245.res;
        final f2j.Closure x247 = x246;
        class Fun248 extends f2j.Closure
        {
          f2j.Closure x249 = this;
          public void apply ()
          {
            final java.lang.String x250 = (java.lang.String) x249.arg;
            final java.lang.String x251 = x250;
            f2j.Closure x252 = x247;
            x252.arg = x251;
            x252.apply();
            final java.lang.String x253 = (java.lang.String) x252.res;
            final java.lang.String x254 = x253;
            res = x254;
          }
        }
        res = new Fun248();
      }
    }
    final f2j.tuples.Tuple2 x255 = new f2j.tuples.Tuple2(new Fun229(), new Fun241());
    final f2j.tuples.Tuple2 x188 = x25;
    final f2j.tuples.Tuple2 x189 = x188;
    final f2j.tuples.Tuple2 x190 = (f2j.tuples.Tuple2) x189._1;
    final f2j.tuples.Tuple2 x191 = x190;
    final f2j.Closure x192 = (f2j.Closure) x191._1;
    final f2j.Closure x193 = x192;
    class Fun194 extends f2j.Closure
    {
      f2j.Closure x195 = this;
      public void apply ()
      {
        final java.lang.Integer x196 = (java.lang.Integer) x195.arg;
        final java.lang.Integer x197 = x196;
        f2j.Closure x198 = x193;
        x198.arg = x197;
        x198.apply();
        final java.lang.Integer x199 = (java.lang.Integer) x198.res;
        final java.lang.Integer x200 = x199;
        res = x200;
      }
    }
    final f2j.tuples.Tuple2 x201 = x188;
    final f2j.tuples.Tuple2 x202 = (f2j.tuples.Tuple2) x201._1;
    final f2j.tuples.Tuple2 x203 = x202;
    final f2j.Closure x204 = (f2j.Closure) x203._2;
    final f2j.Closure x205 = x204;
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
        final f2j.Closure x211 = (f2j.Closure) x210.res;
        final f2j.Closure x212 = x211;
        class Fun213 extends f2j.Closure
        {
          f2j.Closure x214 = this;
          public void apply ()
          {
            final java.lang.Integer x215 = (java.lang.Integer) x214.arg;
            final java.lang.Integer x216 = x215;
            f2j.Closure x217 = x212;
            x217.arg = x216;
            x217.apply();
            final java.lang.Integer x218 = (java.lang.Integer) x217.res;
            final java.lang.Integer x219 = x218;
            res = x219;
          }
        }
        res = new Fun213();
      }
    }
    final f2j.tuples.Tuple2 x220 = new f2j.tuples.Tuple2(new Fun194(), new Fun206());
    f2j.Closure x221 = new Fun53();
    x221.arg = x220;
    x221.apply();
    final f2j.Closure x222 = (f2j.Closure) x221.res;
    f2j.Closure x256 = x222;
    x256.arg = x255;
    x256.apply();
    final f2j.tuples.Tuple2 x257 = (f2j.tuples.Tuple2) x256.res;
    final f2j.tuples.Tuple2 x258 = x257;
    final f2j.tuples.Tuple2 x259 = x258;
    final f2j.tuples.Tuple2 x260 = x259;
    final f2j.Closure x261 = (f2j.Closure) x260._1;
    final f2j.Closure x262 = x261;
    class Fun263 extends f2j.Closure
    {
      f2j.Closure x264 = this;
      public void apply ()
      {
        final java.lang.Integer x265 = (java.lang.Integer) x264.arg;
        final java.lang.Integer x266 = x265;
        f2j.Closure x267 = x262;
        x267.arg = x266;
        x267.apply();
        final f2j.tuples.Tuple2 x268 = (f2j.tuples.Tuple2) x267.res;
        final f2j.tuples.Tuple2 x269 = x268;
        final f2j.tuples.Tuple2 x270 = x269;
        final Object x271 = (Object) x270._1;
        final Object x272 = x271;
        final f2j.tuples.Tuple2 x273 = x269;
        final Object x274 = (Object) x273._2;
        final Object x275 = x274;
        final f2j.tuples.Tuple2 x276 = new f2j.tuples.Tuple2(x272, x275);
        res = x276;
      }
    }
    final f2j.tuples.Tuple2 x277 = x259;
    final f2j.Closure x278 = (f2j.Closure) x277._2;
    final f2j.Closure x279 = x278;
    class Fun280 extends f2j.Closure
    {
      f2j.Closure x281 = this;
      public void apply ()
      {
        final f2j.tuples.Tuple2 x282 = (f2j.tuples.Tuple2) x281.arg;
        final f2j.tuples.Tuple2 x283 = x282;
        final f2j.tuples.Tuple2 x284 = x283;
        final java.lang.Integer x285 = (java.lang.Integer) x284._1;
        final java.lang.Integer x286 = x285;
        final f2j.tuples.Tuple2 x287 = x283;
        final java.lang.String x288 = (java.lang.String) x287._2;
        final java.lang.String x289 = x288;
        final f2j.tuples.Tuple2 x290 = new f2j.tuples.Tuple2(x286, x289);
        f2j.Closure x291 = x279;
        x291.arg = x290;
        x291.apply();
        final f2j.Closure x292 = (f2j.Closure) x291.res;
        final f2j.Closure x293 = x292;
        class Fun294 extends f2j.Closure
        {
          f2j.Closure x295 = this;
          public void apply ()
          {
            final f2j.tuples.Tuple2 x296 = (f2j.tuples.Tuple2) x295.arg;
            final f2j.tuples.Tuple2 x297 = x296;
            final f2j.tuples.Tuple2 x298 = x297;
            final java.lang.Integer x299 = (java.lang.Integer) x298._1;
            final java.lang.Integer x300 = x299;
            final f2j.tuples.Tuple2 x301 = x297;
            final java.lang.String x302 = (java.lang.String) x301._2;
            final java.lang.String x303 = x302;
            final f2j.tuples.Tuple2 x304 = new f2j.tuples.Tuple2(x300, x303);
            f2j.Closure x305 = x293;
            x305.arg = x304;
            x305.apply();
            final f2j.tuples.Tuple2 x306 = (f2j.tuples.Tuple2) x305.res;
            final f2j.tuples.Tuple2 x307 = x306;
            final f2j.tuples.Tuple2 x308 = x307;
            final Object x309 = (Object) x308._1;
            final Object x310 = x309;
            final f2j.tuples.Tuple2 x311 = x307;
            final Object x312 = (Object) x311._2;
            final Object x313 = x312;
            final f2j.tuples.Tuple2 x314 = new f2j.tuples.Tuple2(x310, x313);
            res = x314;
          }
        }
        res = new Fun294();
      }
    }
    final f2j.tuples.Tuple2 x315 = new f2j.tuples.Tuple2(new Fun263(), new Fun280());
    f2j.Closure x316 = new Fun109();
    x316.arg = x315;
    x316.apply();
    final f2j.tuples.Tuple2 x317 = (f2j.tuples.Tuple2) x316.res;
    final f2j.tuples.Tuple2 x318 = x317;
    final java.lang.String x319 = (java.lang.String) x318._2;
    final java.lang.String x320 = x319;
    return x320;
  }
  public static void main (String[] args)
  {
    System.out.println(apply());
  }
}