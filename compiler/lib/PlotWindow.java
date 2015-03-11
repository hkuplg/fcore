package lib;

import javax.swing.JFrame;

import java.awt.Graphics;
import java.awt.Color;
import java.awt.image.BufferedImage;

public class PlotWindow extends JFrame
{
    static final int SIZE = 600;
    BufferedImage bufferedImage = new BufferedImage(SIZE, SIZE, BufferedImage.TYPE_INT_RGB);
    static PlotWindow plotWindow;
    static double lx, rx, ty, by;

    private PlotWindow(int lx, int ty, int rx, int by)
    {
        PlotWindow.lx = (double)lx / 100;
        PlotWindow.ty = (double)ty / 100;
        PlotWindow.rx = (double)rx / 100;
        PlotWindow.by = (double)by / 100;
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setSize(SIZE, SIZE);
        setVisible(true);
    }

    public void paint(Graphics g)
    {
        super.paint(g);
        g.drawImage(bufferedImage, 0, 0, null);
    }

    public static void create(int lx, int ty, int rx, int by)
    {
        plotWindow = new PlotWindow(lx, ty, rx, by);
    }

    public static int setRGBColor(int x, int y, Color c)
    {
        plotWindow.setRGBColor(x, y, c.getRGB());
        return 0;
    }

    void setRGBColor(int x, int y, int c)
    {
        bufferedImage.setRGB(x, y, c);
        if (x == SIZE - 1 && y == SIZE - 1)
            repaint();
    }

    public static Complex toComplex(int x, int y) 
    {
        double xscale = (double)x / SIZE, yscale = (double)y / SIZE;
        return new Complex(xscale * (rx-lx) + lx, yscale * (ty-by) + by);
    }
}

