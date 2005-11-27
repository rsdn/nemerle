using System;
using System.Collections.Generic;

class ray {
    static double delta=1.49011611938476562e-08, infinity=double.PositiveInfinity;
    struct Vec {
        public double x, y, z;
        public Vec(double x2, double y2, double z2) { x=x2; y=y2; z=z2; }
    }

    static Vec add(Vec a, Vec b) { return new Vec(a.x+b.x, a.y+b.y, a.z+b.z); }
    static Vec sub(Vec a, Vec b) { return new Vec(a.x-b.x, a.y-b.y, a.z-b.z); }
    static Vec scale(double s, Vec a) { return new Vec(s*a.x, s*a.y, s*a.z); }
    static double dot(Vec a, Vec b) { return a.x*b.x + a.y*b.y + a.z*b.z; }
    static Vec unitise(Vec a) { return scale(1 / Math.Sqrt(dot(a, a)), a); }

    class Ray {
        public Vec orig, dir;
        public Ray(Vec o, Vec d) { orig=o; dir=d; }
    }
        
    class Hit {
        public double lambda;
        public Vec normal;
        public Hit(double l, Vec n) { lambda=l; normal=n; }
    }
        
    abstract class Scene {
        abstract public Hit intersect(Hit i, Ray ray);
    }
        
    class Sphere : Scene {
        public Vec center;
        public double radius;
        public Sphere(Vec c, double r) { center=c; radius=r; }
        public double ray_sphere(Ray ray) {
            Vec v = sub(center, ray.orig);
            double b = dot(v, ray.dir),
                disc = b*b - dot(v, v) + radius*radius;
            if (disc < 0) return infinity;
            double d = Math.Sqrt(disc), t2 = b+d;
            if (t2 < 0) return infinity;
            double t1 = b-d;
            return (t1 > 0 ? t1 : t2);
        }
        public override Hit intersect(Hit i, Ray ray) {
            double l = ray_sphere(ray);
            if (l >= i.lambda) return i;
            Vec n = add(ray.orig, sub(scale(l, ray.dir), center));
            return new Hit(l, unitise(n));
        }
    }
        
    class Group : Scene {
        public Sphere bound;
        public List<Scene> objs;
        public Group(Sphere b) {
            bound = b;
            objs = new List<Scene>();
        }
        public override Hit intersect(Hit i, Ray ray) {
            double l = bound.ray_sphere(ray);
            if (l >= i.lambda) return i;
			foreach (Scene scene in objs) {
                i = scene.intersect(i, ray);
            }
            return i;
        }
    }
        
    static double ray_trace(Vec light, Ray ray, Scene scene) {
        Hit i = scene.intersect(new Hit(infinity, new Vec(0, 0, 0)), ray);
        if (i.lambda == infinity) return 0;
        Vec o = add(ray.orig, add(scale(i.lambda, ray.dir),
                                  scale(delta, i.normal)));
        double g = dot(i.normal, light);
        if (g >= 0) return 0.0;
        Ray sray = new Ray(o, scale(-1, light));
        Hit si = scene.intersect(new Hit(infinity, new Vec(0, 0, 0)), sray);
        return (si.lambda == infinity ? -g : 0);
    }
    static Scene create(int level, Vec c, double r) {
        Sphere sphere = new Sphere(c, r);
        if (level == 1) return sphere;
        Group group = new Group(new Sphere(c, 3*r));
        group.objs.Add(sphere);
        double rn = 3*r/Math.Sqrt(12);
        for (int dz=-1; dz<=1; dz+=2)
            for (int dx=-1; dx<=1; dx+=2) {
                Vec c2 = new Vec(c.x+dx*rn, c.y+rn, c.z+dz*rn);
                group.objs.Add(create(level-1, c2, r/2));
            }
        return group;
    }
    static void run(int n, int level, int ss) {
        Scene scene = create(level, new Vec(0, -1, 0), 1);
        System.Console.Write("P5\n"+n+" "+n+"\n255\n");
        for (int y=n-1; y>=0; --y)
            for (int x=0; x<n; ++x) {
                double g=0;
                for (int dx=0; dx<ss; ++dx)
                    for (int dy=0; dy<ss; ++dy) {
                        Vec d = new Vec(x+dx*1.0/ss-n/2.0, y+dy*1.0/ss-n/2.0, n);
                        Ray ray = new Ray(new Vec(0, 0, -4), unitise(d));
                        g += ray_trace(unitise(new Vec(-1, -3, 2)),
                                       ray, scene);
                    }
                System.Console.Write((char)(.5+255*g/(ss*ss)));
            }
    }

    public static void Main(String[] args) {
        run(int.Parse(args[1]),
                        int.Parse(args[0]), 4);
    }
}
