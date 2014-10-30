public class Cps_knapsack
{
  static int apply ()
  {
    class Fun2 extends f2j.Closure
    {
      f2j.Closure x3 = this;
      {
        class Fun5 extends f2j.Closure
        {
          f2j.Closure x6 = this;
          public void apply ()
          {
            final java.lang.Integer x7 = (java.lang.Integer) x6.x;
            final java.lang.Integer x4 = (java.lang.Integer) x3.x;
            final java.lang.Boolean x9 = x4 > x7;
            java.lang.Integer ifres8;
            if (x9)
            {
              ifres8 = x4;
            }
            else
            {
              ifres8 = x7;
            }
            out = ifres8;
          }
          public f2j.Closure clone ()
          {
            f2j.Closure c = new Fun5();
            c.x = this.x;
            c.apply();
            return (f2j.Closure) c;
          }
        }
        f2j.Closure x5 = new Fun5();
        out = x5;
      }
      public void apply ()
      {
      }
      public f2j.Closure clone ()
      {
        f2j.Closure c = new Fun2();
        c.x = this.x;
        c.apply();
        return (f2j.Closure) c;
      }
    }
    f2j.Closure x2 = new Fun2();
    final f2j.Closure x0 = x2;
    class Fun12 extends f2j.Closure
    {
      f2j.Closure x13 = this;
      {
        class Fun15 extends f2j.Closure
        {
          f2j.Closure x16 = this;
          {
            class Fun18 extends f2j.Closure
            {
              f2j.Closure x19 = this;
              public void apply ()
              {
                final java.lang.Integer x20 = (java.lang.Integer) x19.x;
                final f2j.FunctionalList x17 = (f2j.FunctionalList) x16.x;
                final f2j.FunctionalList x14 = (f2j.FunctionalList) x13.x;
                class Fun22 extends f2j.Closure
                {
                  f2j.Closure x23 = this;
                  public void apply ()
                  {
                    final f2j.Closure x24 = (f2j.Closure) x23.x;
                    class Fun28 extends f2j.Closure
                    {
                      f2j.Closure x29 = this;
                      public void apply ()
                      {
                        final java.lang.Integer x30 = (java.lang.Integer) x29.x;
                        out = x30;
                      }
                      public f2j.Closure clone ()
                      {
                        f2j.Closure c = new Fun28();
                        c.x = this.x;
                        c.apply();
                        return (f2j.Closure) c;
                      }
                    }
                    f2j.Closure x28 = new Fun28();
                    f2j.Closure x27 = x24;
                    x27.x = x28;
                    final f2j.Closure temp31 = (f2j.Closure) x27.out;
                    final java.lang.Integer x32 = x14.length();
                    f2j.Closure x26 = temp31;
                    x26.x = x32;
                    final f2j.Closure temp33 = (f2j.Closure) x26.out;
                    f2j.Closure x25 = temp33;
                    x25.x = x20;
                    x25.apply();
                    final java.lang.Integer temp34 = (java.lang.Integer) x25.out;
                    out = temp34;
                  }
                  public f2j.Closure clone ()
                  {
                    f2j.Closure c = new Fun22();
                    c.x = this.x;
                    c.apply();
                    return (f2j.Closure) c;
                  }
                }
                f2j.Closure x22 = new Fun22();
                class Fun36 extends f2j.Closure
                {
                  f2j.Closure x35 = this;
                  {
                    class Fun38 extends f2j.Closure
                    {
                      f2j.Closure x39 = this;
                      {
                        class Fun41 extends f2j.Closure
                        {
                          f2j.Closure x42 = this;
                          public void apply ()
                          {
                            final java.lang.Integer x43 = (java.lang.Integer) x42.x;
                            final java.lang.Integer x40 = (java.lang.Integer) x39.x;
                            final f2j.Closure x37 = (f2j.Closure) x35.x;
                            final java.lang.Boolean x45 = x40 == 0;
                            final java.lang.Boolean x46 = x43 == 0;
                            final java.lang.Boolean x47 = x45 || x46;
                            java.lang.Integer ifres44;
                            if (x47)
                            {
                              f2j.Closure x48 = x37;
                              x48.x = 0;
                              x48.apply();
                              final java.lang.Integer temp49 = (java.lang.Integer) x48.out;
                              ifres44 = temp49;
                            }
                            else
                            {
                              final java.lang.Integer x51 = x40 - 1;
                              final java.lang.Integer x52 = x17.<java.lang.Integer>at(x51);
                              final java.lang.Boolean x53 = x52 > x43;
                              java.lang.Integer ifres50;
                              if (x53)
                              {
                                f2j.Closure x56 = x35;
                                x56.x = x37;
                                final f2j.Closure temp57 = (f2j.Closure) x56.out;
                                final java.lang.Integer x58 = x40 - 1;
                                f2j.Closure x55 = temp57;
                                x55.x = x58;
                                final f2j.Closure temp59 = (f2j.Closure) x55.out;
                                f2j.Closure x54 = temp59;
                                x54.x = x43;
                                x54.apply();
                                final java.lang.Integer temp60 = (java.lang.Integer) x54.out;
                                ifres50 = temp60;
                              }
                              else
                              {
                                class Fun64 extends f2j.Closure
                                {
                                  f2j.Closure x65 = this;
                                  public void apply ()
                                  {
                                    final java.lang.Integer x66 = (java.lang.Integer) x65.x;
                                    class Fun70 extends f2j.Closure
                                    {
                                      f2j.Closure x71 = this;
                                      public void apply ()
                                      {
                                        final java.lang.Integer x72 = (java.lang.Integer) x71.x;
                                        f2j.Closure x75 = x0;
                                        x75.x = x66;
                                        final f2j.Closure temp76 = (f2j.Closure) x75.out;
                                        final java.lang.Integer x77 = x40 - 1;
                                        final java.lang.Integer x78 = x14.<java.lang.Integer>at(x77);
                                        final java.lang.Integer x79 = x78 + x72;
                                        f2j.Closure x74 = temp76;
                                        x74.x = x79;
                                        x74.apply();
                                        final java.lang.Integer temp80 = (java.lang.Integer) x74.out;
                                        f2j.Closure x73 = x37;
                                        x73.x = temp80;
                                        x73.apply();
                                        final java.lang.Integer temp81 = (java.lang.Integer) x73.out;
                                        out = temp81;
                                      }
                                      public f2j.Closure clone ()
                                      {
                                        f2j.Closure c = new Fun70();
                                        c.x = this.x;
                                        c.apply();
                                        return (f2j.Closure) c;
                                      }
                                    }
                                    f2j.Closure x70 = new Fun70();
                                    f2j.Closure x69 = x35.clone();
                                    x69.x = x70;
                                    final f2j.Closure temp82 = (f2j.Closure) x69.out;
                                    final java.lang.Integer x83 = x40 - 1;
                                    f2j.Closure x68 = temp82;
                                    x68.x = x83;
                                    final f2j.Closure temp84 = (f2j.Closure) x68.out;
                                    final java.lang.Integer x85 = x40 - 1;
                                    final java.lang.Integer x86 = x17.<java.lang.Integer>at(x85);
                                    final java.lang.Integer x87 = x43 - x86;
                                    f2j.Closure x67 = temp84;
                                    x67.x = x87;
                                    x67.apply();
                                    final java.lang.Integer temp88 = (java.lang.Integer) x67.out;
                                    out = temp88;
                                  }
                                  public f2j.Closure clone ()
                                  {
                                    f2j.Closure c = new Fun64();
                                    c.x = this.x;
                                    c.apply();
                                    return (f2j.Closure) c;
                                  }
                                }
                                f2j.Closure x64 = new Fun64();
                                f2j.Closure x63 = x35.clone();
                                x63.x = x64;
                                final f2j.Closure temp89 = (f2j.Closure) x63.out;
                                final java.lang.Integer x90 = x40 - 1;
                                f2j.Closure x62 = temp89;
                                x62.x = x90;
                                final f2j.Closure temp91 = (f2j.Closure) x62.out;
                                f2j.Closure x61 = temp91;
                                x61.x = x43;
                                x61.apply();
                                final java.lang.Integer temp92 = (java.lang.Integer) x61.out;
                                ifres50 = temp92;
                              }
                              ifres44 = ifres50;
                            }
                            out = ifres44;
                          }
                          public f2j.Closure clone ()
                          {
                            f2j.Closure c = new Fun41();
                            c.x = this.x;
                            c.apply();
                            return (f2j.Closure) c;
                          }
                        }
                        f2j.Closure x41 = new Fun41();
                        out = x41;
                      }
                      public void apply ()
                      {
                      }
                      public f2j.Closure clone ()
                      {
                        f2j.Closure c = new Fun38();
                        c.x = this.x;
                        c.apply();
                        return (f2j.Closure) c;
                      }
                    }
                    f2j.Closure x38 = new Fun38();
                    out = x38;
                  }
                  public void apply ()
                  {
                  }
                  public f2j.Closure clone ()
                  {
                    f2j.Closure c = new Fun36();
                    c.x = this.x;
                    c.apply();
                    return (f2j.Closure) c;
                  }
                }
                f2j.Closure x36 = new Fun36();
                f2j.Closure x21 = x22;
                x21.x = x36;
                x21.apply();
                final java.lang.Integer temp93 = (java.lang.Integer) x21.out;
                out = temp93;
              }
              public f2j.Closure clone ()
              {
                f2j.Closure c = new Fun18();
                c.x = this.x;
                c.apply();
                return (f2j.Closure) c;
              }
            }
            f2j.Closure x18 = new Fun18();
            out = x18;
          }
          public void apply ()
          {
          }
          public f2j.Closure clone ()
          {
            f2j.Closure c = new Fun15();
            c.x = this.x;
            c.apply();
            return (f2j.Closure) c;
          }
        }
        f2j.Closure x15 = new Fun15();
        out = x15;
      }
      public void apply ()
      {
      }
      public f2j.Closure clone ()
      {
        f2j.Closure c = new Fun12();
        c.x = this.x;
        c.apply();
        return (f2j.Closure) c;
      }
    }
    f2j.Closure x12 = new Fun12();
    final f2j.Closure x10 = x12;
    final f2j.FunctionalList x97 = new f2j.FunctionalList();
    final f2j.FunctionalList x98 = new <java.lang.Integer, f2j.FunctionalList> f2j.FunctionalList(4, x97);
    final f2j.FunctionalList x99 = new <java.lang.Integer, f2j.FunctionalList> f2j.FunctionalList(3, x98);
    final f2j.FunctionalList x100 = new <java.lang.Integer, f2j.FunctionalList> f2j.FunctionalList(9, x99);
    f2j.Closure x96 = x10;
    x96.x = x100;
    final f2j.Closure temp101 = (f2j.Closure) x96.out;
    final f2j.FunctionalList x102 = new f2j.FunctionalList();
    final f2j.FunctionalList x103 = new <java.lang.Integer, f2j.FunctionalList> f2j.FunctionalList(8, x102);
    final f2j.FunctionalList x104 = new <java.lang.Integer, f2j.FunctionalList> f2j.FunctionalList(5, x103);
    final f2j.FunctionalList x105 = new <java.lang.Integer, f2j.FunctionalList> f2j.FunctionalList(2, x104);
    f2j.Closure x95 = temp101;
    x95.x = x105;
    final f2j.Closure temp106 = (f2j.Closure) x95.out;
    f2j.Closure x94 = temp106;
    x94.x = 10;
    x94.apply();
    final java.lang.Integer temp107 = (java.lang.Integer) x94.out;
    final java.lang.Integer x11 = temp107;
    final java.lang.Integer x1 = x11;
    return x1;
  }
  public static void main (String[] args)
  {
    System.out.println(apply());
  }
}