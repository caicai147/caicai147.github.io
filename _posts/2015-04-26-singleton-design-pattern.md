---
layout: post
title: 单例模式的七种写法
categories: java之设计模式 java之面向对象 深入学习之面向对象 深入学习之设计模式
tags: java 设计模式 面向对象
---


转载：[http://cantellow.iteye.com/blog/838473](http://cantellow.iteye.com/blog/838473)

 

第一种（懒汉，线程不安全）
---

    //Java代码
    
    public class Singleton{
        private static Singleton instance;
        private Singleton(){};
    
        public static Singleton getInstance(){
            if(instance ==null)
                instance = new Singleton();
            }
            return instance;
        }
    }

这种写法 lazy loading 很明显，但是致命的是在多线程时候不能正常工作


第二种（懒汉，线程安全）
---

    //Java代码
    public class Singleton{
        private static Singleton instance;
        private Singleton(){}
        public static synchronized Singleton getInstance(){
            if(instance == null){
                instance = new Singleton();
            }
            return instance;
        }
    }

这种写法能够在多线程中很好的工作，而且看起来它也具备很好的 lazy loading ，但是遗憾的是，效率很低，99%情况下不需要同步

可以看出这两种 懒汉模式 都是在需要创建的时候再去创建一个单例实例，而下面要说的 饿汉模式 是在一开始的时候就去创建一个单例实例


第三种（饿汉）
---

    //Java代码
    public class Singleton{
        private static Singleton instance = new Singleton();
        private Singleton(){}
        public static Singleton getInstance(){
            return instance;
        }
    }

这种方式基于 classloder 机制避免了多线程的同步问题，不过，instance 在类装载时就实例化，虽然导致类装载的原因很多，在单例模式中大多数都是调用 getInstance方法，但是也不确定有其他的方式（或者其他的静态方法）导致类装载，这时候初始化 instance显然没有达到 lazy loading的效果


第四种（饿汉，变种）
---

    //Java代码
    public class Singleton{
        private Singelton instance = null;
        static{
            instance = new Singleton();
        }
        private Singleton();
        public static Singleton getInstance(){
            return this.instance;
        }
    }

表面上看起来差别挺大，其实和第三种方式差不多，都是在类初始化即实例化 instance


第五种（静态内部类）
---

    //Java代码
    public class Singleton{
        private static class SingletonHolder{
            private static final Singleton INSTANCE = new Singleton();
        }
        private Singleton(){}
        public static final Singleton getInstance(){
            return SingletonHolder.INSTANCE;
        }
    }

这种方式虽然同样利用了 classloder 的机制来保证初始化 instance 时只有一个线程，它跟第三种和第四种方式不同的是（很细微的差别）：第三种和第四种方式是只要 Singleton类被加载了，那么instance 就会被实例化（没有达到 lazy loading效果），而这种方式是 Singleton类被加载了，instance 不一定被初始化。因为 SingletonHolder类没有被主动使用，只有显式通过调用 getInstance 方法时，才会显式装载 SingletonHolder类，从而实例化 instance。

想象一下，如果实例化 instance很消耗资源，我想它延迟加载，另外一方面，我不希望在 Singleton类加载时就实例化，因为我不能确定 Singleton类还可能在其他的地方呗主动使用从而加载，那么这个时候实例化 instance 显然是不合适的。这个时候，这种方式相比第三和第四种方式就显得很合理


第六种（枚举）
---

    //Java代码
    public enum Singleton{
        INSTANCE;
        public void whateverMethod(){
        }
    }

这种方式是《Effective Java》的作者 Josh Bloch 提倡的方式，它不仅能避免多线程同步问题，而且还能方式反序列化重新创建新的对象，可谓是很坚强的壁垒啊！！

不过，个人认为由于1.5中才加入enum特性，用这种方式写不免让人感觉生疏，在实际工作中，我也很少看见有人这么写过。


第七种（双重校验锁）
---

    public class Singleton{
        private volatile static Singleton singleton;
        private Singleton(){}
        public static Singleton getSingleton(){
            if(singleton == null){
                synchronized(Singleton.class){
                    if(singleton == null){
                        singleton = new Singleton();
                    }
                }
            }
            return singleton;
        }
    }

这个是第二种方式的升级版，俗称双重检查锁定。

在JDK1.5之后，双重检查锁定才能够正常达到单例效果。

 

总结
===

有两个问题需要注意：

* 如果单例由不同的类装载器装入，那便有可能存在多个单例类的实例。假定不是远端存取，例如一些 servlet容器对每个 servlet使用完全不同的类装载器，这样的话如果有两个 servlet访问一个单例类，它们就都会有各自的实例
* 如果Singleton实现了java.io.Serializable接口，那么这个类的实例就可能被序列化和复原。不管怎么样，如果你序列化一个单例类的对象，接下来复原多个那个对象，那你就会有多个单例类的实例

对第一个问题修复的办法是
---

    private static Class getClass(String classname) throws ClassNotFoundException{
        ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
            if(classLoader == null)
                classLoader = Singleton.class.getClassLoader();
     
            return(classLoader.loaderClass(classname));
        }
    }

第二个问题修复的办法是
---

    public class Singleton implements java.io.Serializable{
        public static Singleton INSTANCE = new Singleton();
    
        protected Object readResolve(){
            return INSTANCE;
        }
    }

对“我”来说，“我”比较喜欢第三种和第五种方式，简单易懂，而且在 JVM 层实现了线程安全（如果不是多个类加载器环境），一般情况下，“我”会使用第三种方式，只要在明确实现 lazy loading 效果时才会使用第五种方式。

另外如果涉及到反序列化创建对象时，“我”会试着使用枚举的方式来实现单例，不过，“我”一直会保证“我”的程序是线程安全的，而且“我”永远不会使用第一和第二种方式，如果有其他特殊的需求，“我”可能会使用第七种方式，毕竟 JDK1.5 已经没有双重检查锁定的问题了

 
[superheizai](http://superheizai.iteye.com/)同学总结的很到位：

>不过一般来说，第一种不算单例，第四种和第三种就是一种，如果算的话，第五种也可以分开写了。所以说，一般单例都是五种写法。懒汉，恶汉，双重校验锁，枚举和静态内部类。