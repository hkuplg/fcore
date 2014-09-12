package hk.hku.cs.f2j;

public class Complex {
    public static Complex ZERO = new Complex(0, 0);

    private double x, y;

    public Complex(double x, double y) {
        this.x = x;
        this.y = y;
    }

    public Complex add(Complex a) {
        return new Complex(x + a.x, y + a.y);
    }

    public Complex multiply(Complex a) {
        return new Complex(x * a.x - y * a.y, y * a.x + x * a.y);
    }

    public Complex sqr() {
        return multiply(this);
    }

    public double magnitude() {
        return Math.sqrt(x * x + y * y);
    }

    public Complex abs() {
        return new Complex(Math.abs(x), Math.abs(y));
    }
}

