#The orbit of a planet is an ellipse with the Sun at one of the two foci.
#A line segment joining a planet and the Sun sweeps out equal areas during equal intervals of time.
#The square of the orbital period of a planet is directly proportional to the cube of the semi-major axis of its orbit.

# Euclidean distance between two points
distance = function(P, Q) {
  sqrt((P[1] - Q[1]) ^ 2 + (P[2] - Q[2]) ^ 2)
}

triangle.area = function(P, Q, F1) {
  abs(
    (P[1] - F1[1]) * (Q[2] - F1[2]) -
      (P[2] - F1[2]) * (Q[1] - F1[1])
  ) / 2
}
# Obtain Y value for given X value in an ellipse with
# semi-major axis a, semi-minor axis b, and centre abscissa Cx
y.ellipse = function(x, a, b, Cx, sign = 1) {
  sign * (b / a) * sqrt(a ^ 2 - (x - Cx) ^ 2)
}

# Orbit parameters
N = 5                                   # Number of orbits
K = 1                                   # Orbit scale factor
R = 3 / 4                               # Orbit axis ratio
F1 = c(-K / 2, 0)                       # Orbit focus
a = sapply(1:N, function(n) K * n)      # Semi-major axis per orbit
b = sapply(1:N, function(n) R * K * n)  # Semi-minor axis per orbit
Cx = (a - K) / 2                        # Centre abscissa per orbit

# Fixed positions along each orbit
orbit.pos = lapply(1:N, function(i) {
  x = seq(Cx[i] - a[i], Cx[i] + a[i], 
          length.out = 500)
  rbind(c(x, rev(x)),
        sapply(c(x, rev(x)), y.ellipse, a[i], b[i], Cx[i]) *
          rep(c(-1, 1), each = 500))
})

# Plot orbits
plot.orbits = function(orbits, F1, colours, t) {
  par(mar = rep(0, 4), bg = "black")
  xs = sapply(orbits, `[`, 1, TRUE)
  ys = sapply(orbits, `[`, 2, TRUE)
  xlim = c(min(xs), max(xs))
  ylim = c(min(ys), max(ys))
  plot(NA, type="n", axes=FALSE,
       xlab="", ylab="", xlim=xlim, ylim=ylim)
  for (i in 1:length(orbits)) {
    lines(orbits[[i]][1, ], orbits[[i]][2, ], lwd=2, col=colours[i])
  }
  points(F1[1], F1[2], pch=16, cex=3, col="white")
  text(xlim[1], ylim[2] * 0.94, paste("t", t, sep=" = "), 
       col="white", cex=1.2, adj=0)
}

plot.orbits(orbit.pos, F1, t=0,
            colours=c("orchid1", "turquoise1", "gold", "lawngreen", "red"))

#Move from current orbit position to the closest position
# resulting in a swept area ≥At
next.orbit.pos = function(P, a, b, Cx, delta, F1, At) {
  area = 0
  x = P[1]
  sign = P[3]
  while (area < At) {
    x = x + sign * delta
    if (x < Cx - a) {
      x = Cx - a
    }
    else if (x > Cx + a) {
      x = Cx + a
    }
    y = y.ellipse(x, a, b, Cx, sign)
    if ((y == 0) | (sign < 0 & y > 0) | (sign > 0 & y < 0)) {
      sign = -sign
    }
    area = triangle.area(P, c(x, y), F1)
  }
  c(x, y, sign)
}

# ===========================
# Simulation parameters
# ===========================

D = 1e-3         # 计算步长，原来 5e-5，增大后计算更快
At = 0.03         # 每帧扫过面积，减小后运动更平滑
Nt = 1000         # 总帧数

COL = c(
  "orchid1",
  "turquoise1",
  "gold",
  "lawngreen",
  "red"
)


# ===========================
# Initialise orbit positions
# ===========================

positions = rbind(
  sapply(orbit.pos, `[`, TRUE, 1),
  sign = 1
)


# 用 list 保存所有帧的位置
frames = vector("list", Nt)


# ===========================
# 1. 先计算所有轨道位置
# ===========================

cat("Calculating...\n")

for (i in 1:Nt) {
  
  # 保存当前帧
  frames[[i]] = positions
  
  # 计算下一帧
  positions = sapply(
    1:ncol(positions),
    function(j) {
      
      next.orbit.pos(
        positions[, j],
        a[j],
        b[j],
        Cx[j],
        D,
        F1,
        At
      )
      
    }
  )
  
  # 显示计算进度
  if (i %% 50 == 0) {
    cat("Frame:", i, "/", Nt, "\n")
  }
}


cat("Calculation finished!\n")


# ===========================
# 2. 打开动画窗口
# ===========================

windows(
  width = 8,
  height = 6,
  buffered = TRUE
)


# ===========================
# 3. 播放动画
# ===========================

for (i in 1:Nt) {
  
  # 暂时停止刷新，减少闪烁
  dev.hold()
  
  # 画轨道
  plot.orbits(
    orbit.pos,
    F1,
    COL,
    i
  )
   
  # 画5个行星
  points(
    frames[[i]][1, ],
    frames[[i]][2, ],
    col = COL,
    pch = 16,
    cex = 2
  )
 
  # 一次性刷新这一帧
  dev.flush()
  
  # 控制播放速度
  Sys.sleep(0.008)
}