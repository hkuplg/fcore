package lib;

import java.awt.*;
import javax.swing.*;
import java.awt.image.BufferedImage;


public class DrawFrame extends JFrame {

    private static int level;
    private static int x = 0, y = 0, SIZE = 4 * 128, dist;
    private static BufferedImage bufferedImage = new BufferedImage(SIZE, SIZE, BufferedImage.TYPE_INT_RGB);
    private static DrawFrame s = null;

    public static void create(int level, int dist) {
        DrawFrame.level = level;
        DrawFrame.dist = dist;
        x = 2 * dist;
        y = dist;
        s = new DrawFrame();
    }

    public static void refresh() {
        s.repaint();
    }

    private DrawFrame() {
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setSize(SIZE, SIZE);
        setVisible(true);
    }

    public void paint(Graphics g)
    {
        super.paint(g);
        g.drawImage(bufferedImage, 0, 0, null);
    }

    public static void lineTo(int deltaX, int deltaY) {
        bufferedImage.getGraphics().drawLine(x, y, x + deltaX, y + deltaY);
        x += deltaX;
        y += deltaY;
    }
}

