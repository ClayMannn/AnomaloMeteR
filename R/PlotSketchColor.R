PlotSketchColor <-  function (df,            col = "gray80",            border = NULL,            main = "",  bodyxlab = "", bodyylab  = ""    ,     x.offset = 0,            leg = FALSE,            hip= FALSE,            torso= FALSE,            waist= FALSE,headneck= FALSE,            arm= FALSE,            shoulder= FALSE,            body= FALSE,            breastbust= FALSE,            bback= FALSE  )
{
  library(plotrix)
  library(ellipse)
  # library(ggplot2)
  # library(ggforce)
  if (is.null(leg)) {
    leg = FALSE
  }

  if (is.null(hip)) {
    hip = FALSE
  }

  if (is.null(torso)) {
    torso = FALSE
  }

  if (is.null(waist)) {
    waist = FALSE
  }

  if (is.null(headneck)) {
    headneck = FALSE
  }

  if (is.null(arm)) {
    arm = FALSE
  }

  if (is.null(shoulder)) {
    shoulder = FALSE
  }

  if (is.null(body)) {
    body = FALSE
  }

  if (is.null(breastbust)) {
    breastbust = FALSE
  }

  if (is.null(bback)) {
    bback = FALSE
  }




  if(leg | hip | torso | waist | headneck | arm | shoulder | body | breastbust | bback){givenoutliers <-  c( "outliers found in:")}

  height = df$BS_HT

  calf.length = df["BS_KNEE_HT"]
  calf.width.R = df$BS_CALF_GTH_R / pi
  calf.width.L = df$BS_CALF_GTH_L / pi
  calf.width = (calf.width.R  + calf.width.L )/2
  thigh.length = df["BS_HIP_HT"] - df["BS_KNEE_HT"]
  thigh.width.R = df$BS_THIGH_GTH_L_HZ / pi
  thigh.width.L = df$BS_THIGH_GTH_R_HZ / pi
  hip.width = df["BS_HIP_GTH"] / pi
  thigh.width = (thigh.width.R + thigh.width.L) /2

  chest.width = df["BS_BUST_CHEST_GTH"] / pi
  belly.width = df["BS_MAX_BELLY_CIRC"] / pi

  head.height = df["BS_HEAD_HT"]
  head.width = df["BS_HEAD_CIRC"] / pi

  up.arm.length.R = df$BS_UP_ARM_LTH_L
  up.arm.length.L = df$BS_UP_ARM_LTH_R
  up.arm.length = (up.arm.length.R +up.arm.length.L ) /2
  up.arm.width.R = df$BS_UP_ARM_DIAM_L
  up.arm.width.L = df$BS_UP_ARM_DIAM_R
  up.arm.width = (up.arm.width.R + up.arm.width.L) /2
  forearm.length.R =  df$BS_FOREARM_LTH_R
  forearm.length.L =  df$BS_FOREARM_LTH_L
  forearm.length = (forearm.length.R + forearm.length.L) / 2
  forearm.width.R = df$BS_FOREARM_GTH_R / pi
  forearm.width.L = df$BS_FOREARM_GTH_L / pi
  forearm.width = (forearm.width.R + forearm.width.L) / 2


  shoulder.height = df["BS_NECK_HT"]
  shoulder.angle.R = df$BS_SHOULDER_ANGLE_R
  shoulder.angle.L = df$BS_SHOULDER_ANGLE_L
  shoulder.angle = (shoulder.angle.R + shoulder.angle.L) /2
  #if it is multi dimuension it makes mean
  height <- mean(height, na.rm=TRUE)
  calf.length <- mean(calf.length$BS_KNEE_HT, na.rm=TRUE)
  calf.width.R <- mean(calf.width.R, na.rm=TRUE)
  calf.width.L <- mean(calf.width.L, na.rm=TRUE)
  thigh.length <- mean(thigh.length$BS_HIP_HT, na.rm=TRUE)
  thigh.width.R <- mean(thigh.width.R, na.rm=TRUE)
  thigh.width.L <- mean(thigh.width.L, na.rm=TRUE)
  hip.width <- mean(hip.width$BS_HIP_GTH, na.rm=TRUE)
  chest.width <- mean(chest.width$BS_BUST_CHEST_GTH, na.rm=TRUE)
  belly.width <- mean(belly.width$BS_MAX_BELLY_CIRC, na.rm=TRUE)
  head.height <- mean(head.height$BS_HEAD_HT, na.rm=TRUE)
  head.width <- mean(head.width$BS_HEAD_CIRC, na.rm=TRUE)
  up.arm.length.R <- mean(up.arm.length.R, na.rm=TRUE)
  up.arm.length.L <- mean(up.arm.length.L, na.rm=TRUE)
  up.arm.width.R <- mean(up.arm.width.R, na.rm=TRUE)
  up.arm.width.L <- mean(up.arm.width.L, na.rm=TRUE)
  forearm.length.R <- mean(forearm.length.R, na.rm=TRUE)
  forearm.length.L <- mean(forearm.length.L, na.rm=TRUE)
  forearm.width.R <- mean(forearm.width.R, na.rm=TRUE)
  forearm.width.L <- mean(forearm.width.L, na.rm=TRUE)
  shoulder.height <- mean(shoulder.height$BS_NECK_HT, na.rm=TRUE)
  shoulder.angle.R <- mean(shoulder.angle.R, na.rm=TRUE)
  shoulder.angle.L <- mean(shoulder.angle.L, na.rm=TRUE)

  ##################################################################
  #col = "gray80"
  col_leg = "gray80"
  if(leg){col_leg = "red"
  givenoutliers <- append(givenoutliers, "leg")}
  col_hip  = "gray80"
  if(hip){col_hip = "red"
  givenoutliers <- append(givenoutliers, "hip")}
  col_torso  = "gray80"
  if(torso){col_torso = "red"
  givenoutliers <- append(givenoutliers, "torso")}
  col_waist  = "gray80"
  if(waist){col_waist = "red"
  givenoutliers <- append(givenoutliers, "waist")}
  col_headneck = "gray80"
  if(headneck){col_headneck = "red"
  givenoutliers <- append(givenoutliers, "headneck")}
  col_arm = "gray80"
  if(arm){col_arm = "red"
  givenoutliers <- append(givenoutliers, "arm")}
  col_shoulder = "gray80"
  if(shoulder){col_shoulder = "red"
  givenoutliers <- append(givenoutliers, "shoulder")}
  if(breastbust){ col_torso = "red"
  givenoutliers <- append(givenoutliers, "breastbust")}
  if(body){
    col_leg = "red"
    col_hip = "red"
    col_torso = "red"
    col_waist = "red"
    col_headneck = "red"
    col_shoulder = "red"
    col_torso = "red"
    col_arm = "red"
    givenoutliers <- append(givenoutliers, "body")}

  if(bback){
    col_leg = "red"
    col_hip = "red"
    col_torso = "red"
    col_waist = "red"
    col_headneck = "red"
    col_shoulder = "red"
    col_torso = "red"
    col_arm = "red"
    givenoutliers <- append(givenoutliers, "bback")}


  border = NULL
  x.offset = 0
  height = mean(height)
  calf.length = mean(calf.length)
  #calf.width = mean(calf.width)
  thigh.length = mean(thigh.length)
  #thigh.width = mean(thigh.width)
  hip.width = mean(hip.width)
  chest.width = mean(chest.width)
  belly.width = mean(belly.width)
  head.height = mean(head.height)
  head.width = mean(head.width)
  #up.arm.length = mean(up.arm.length)
  #up.arm.width = mean(up.arm.width)
  #forearm.length =  mean(forearm.length)
  #forearm.width = mean(forearm.width)
  shoulder.height = mean(shoulder.height)
  #shoulder.angle = mean(shoulder.angle)
  #

  # }


  calf.width = (calf.width.R  + calf.width.L )/2

  thigh.width = (thigh.width.R + thigh.width.L) /2

  up.arm.length = (up.arm.length.R +up.arm.length.L ) /2

  up.arm.width = (up.arm.width.R + up.arm.width.L) /2

  forearm.length = (forearm.length.R + forearm.length.L) / 2

  forearm.width = (forearm.width.R + forearm.width.L) / 2

  shoulder.angle = (shoulder.angle.R + shoulder.angle.L) /2



  plot(
    c(0, 200),
    c(0, 220),
    type = "n",
    xlab = bodyxlab,
    ylab = bodyylab,
    axes = F,
    main = main,
    yaxs = "i"
  )



  if(leg | hip | torso | waist | headneck | arm | shoulder | body | breastbust | bback){ legend(0,220,givenoutliers, col=list(222,333,333,333,333,333,333,333))}
  box()
  # box coordinates

  max.y.calf = calf.length

  angle.thigh = 5
  dy.thigh = abs(sin(2 * pi / 360 * (90 - angle.thigh)) * thigh.length)
  dx.thigh = abs(cos(2 * pi / 360 * (90 - angle.thigh)) * thigh.length)
  max.y.thigh = calf.length + dy.thigh - 3

  angle.up.arm = 40
  dy.up.arm = abs(sin(2 * pi / 360 * (90 - angle.up.arm)) * up.arm.length.R)
  dx.up.arm = abs(cos(2 * pi / 360 * (90 - angle.up.arm)) * up.arm.length.L)
  min.y.up.arm = shoulder.height - dy.up.arm - 3

  angle.forearm = 5
  dy.forearm = abs(sin(2 * pi / 360 * (90 - angle.forearm)) * forearm.length.R)
  dx.forearm = abs(cos(2 * pi / 360 * (90 - angle.forearm)) * forearm.length.L)


  # calfs Waden

  draw.ellipse(
    90 - dx.thigh / 1.8 + x.offset,
    calf.length / 1.8,
    calf.width.R / 1.8,
    calf.length / 1.8,
    angle = 0,
    col = col_leg,
    border = border
  )
  draw.ellipse(
    110 + dx.thigh / 1.8 + x.offset,
    calf.length / 1.8,
    calf.width.L / 1.8,
    calf.length / 1.8,
    angle = 0,
    col = col_leg,
    border = border
  )


  # hip

  draw.ellipse(
    100 + x.offset,
    max.y.thigh + (shoulder.height - max.y.thigh) / 12,
    hip.width / 2,
    (shoulder.height - max.y.thigh) / 6,
    col = col_hip,
    border = border
  )



  # thighs Oberschenkel

  draw.ellipse(
    90 + x.offset,
    max.y.calf + dy.thigh / 2,
    thigh.width.R / 2,
    thigh.length / 2,
    angle = -angle.thigh,
    col = col_waist,
    border = border
  )
  draw.ellipse(
    110 + x.offset,
    max.y.calf + dy.thigh / 2,
    thigh.width.L / 2,
    thigh.length / 2,
    angle = angle.thigh,
    col = col_waist,
    border = border
  )



  # torso

  draw.ellipse(
    100 + x.offset,
    shoulder.height - (shoulder.height - max.y.thigh) / 3,
    chest.width / 2,
    (shoulder.height - max.y.thigh) / 3,
    col = col_torso,
    border = border
  )
  draw.ellipse(
    100 + x.offset,
    max.y.thigh + (shoulder.height - max.y.thigh) / 3,
    belly.width / 2,
    (shoulder.height - max.y.thigh) / 3,
    col = col_torso,
    border = border
  )





  # head

  draw.ellipse(
    100 + x.offset,
    height - head.height / 2 + 2,
    head.width / 2,
    head.height / 2,
    col = col_headneck,
    border = border
  )



  # upper arms

  draw.ellipse(
    100 - chest.width * 0.45 - dx.up.arm / 2 + x.offset,
    shoulder.height - dy.up.arm / 2 - 3,
    up.arm.width.R / 2,
    up.arm.length.R / 2,
    angle = -angle.up.arm,
    col = col_arm,
    border = border
  )
  draw.ellipse(
    100 + chest.width * 0.45 + dx.up.arm / 2 + x.offset,
    shoulder.height - dy.up.arm / 2 - 3,
    up.arm.width.L / 2,
    up.arm.length.L / 2,
    angle = angle.up.arm,
    col = col_arm,
    border = border
  )



  # forearms

  draw.ellipse(
    100 - chest.width * 0.45 - dx.up.arm + x.offset,
    min.y.up.arm - dy.forearm / 2,
    forearm.width.R / 2,
    forearm.length.R / 2,
    angle = -angle.forearm,
    col = col_arm,
    border = border
  )
  draw.ellipse(
    100 + chest.width * 0.45 + dx.up.arm + x.offset,
    min.y.up.arm - dy.forearm / 2,
    forearm.width.L / 2,
    forearm.length.L / 2,
    angle = angle.forearm,
    col = col_arm,
    border = border
  )



  # hands

  draw.ellipse(
    100 - chest.width * 0.45 - dx.up.arm - dx.forearm / 2 + x.offset,
    min.y.up.arm - dy.forearm - 1,
    forearm.width.R / 2,
    5,
    angle = 0,
    col = col_arm,
    border = border
  )
  draw.ellipse(
    100 + chest.width * 0.45 + dx.up.arm + dx.forearm / 2 + x.offset,
    min.y.up.arm - dy.forearm - 1,
    forearm.width.L / 2,
    5,
    angle = 0,
    col = col_arm,
    border = border
  )



  # shoulders

  draw.ellipse(
    100 - chest.width * 0.4 + x.offset,
    shoulder.height - 3,
    1,
    10,
    col = col_shoulder,
    border = border,
    angle = -90 + shoulder.angle
  )
  draw.ellipse(
    100 + chest.width * 0.4 + x.offset,
    shoulder.height - 3,
    1,
    10,
    col = col_shoulder,
    border = border,
    angle = 90 - shoulder.angle
  )


  #  p <- ggplot() +
  #          xlim(0, 200) + ylim(0, 220) +  # set the plot limits
  #          labs(x = bodyxlab, y = bodyylab, title = main) +  # Set labels and title
  #          theme(axis.title.x = element_blank(),
  #                axis.text.x = element_blank(),
  #                axis.ticks.x = element_blank(),
  #                axis.title.y = element_blank(),
  #                axis.text.y = element_blank(),
  #                axis.ticks.y = element_blank())  +   # Remove axes
  #           # calfs Waden
  #   geom_ellipse(aes(x0 = 90 - dx.thigh / 1.8 + x.offset,
  #                          y0 = calf.length / 1.8,
  #                          a = calf.width.R / 3.6,
  #                          b = calf.length / 3.6,
  #                          angle = 0),
  #                      color = col_leg) +
  #    geom_ellipse(aes(x0 = 110 + dx.thigh / 1.8 + x.offset,
  #                          y0 = calf.length / 1.8,
  #                          a = calf.width.L / 3.6,
  #                          b = calf.length / 3.6,
  #                          angle = 0),
  #                      color = col_leg) +
  #                      # hip
  #   geom_ellipse(aes(x0 = 100 + x.offset,
  #                      y0 = max.y.thigh + (shoulder.height - max.y.thigh) / 12,
  #                      a = hip.width / 3.6,
  #                      b = (shoulder.height - max.y.thigh) / 6,
  #                      angle = 0),
  #                  color = col_hip) +
  #      # thighs Oberschenkel
  #  geom_ellipse(aes(x0 = 90 + x.offset,
  #                      y0 = max.y.calf + dy.thigh / 2,
  #                      a = thigh.width.R / 3.6,
  #                      b = thigh.length / 3.6,
  #                      angle = -angle.thigh),
  #                  color = col_waist) +
  #    geom_ellipse(aes(x0 = 110 + x.offset,
  #                      y0 = max.y.calf + dy.thigh / 2,
  #                      a = thigh.width.L / 3.6,
  #                      b = thigh.length / 3.6,
  #                      angle = angle.thigh),
  #                  color = col_waist)+
  #     # torso
  #     geom_ellipse(aes(x0 = 100 + x.offset,
  #                      y0 = max.y.thigh + (shoulder.height - max.y.thigh) / 3,
  #                      a = chest.width / 3.6,
  #                      b = (shoulder.height - max.y.thigh) / 3,
  #                      angle = 0),
  #                  color = col_torso) +
  #    geom_ellipse(aes(x0 = 100 + x.offset,
  #                      y0 = max.y.thigh + (shoulder.height - max.y.thigh) / 3,
  #                      a = belly.width / 3.6,
  #                      b = (shoulder.height - max.y.thigh) / 3,
  #                      angle = 0),
  #                  color = col_torso)+
  #     # head
  #     geom_ellipse(aes(x0 = 100 + x.offset,
  #                      y0 = height - head.height / 2 + 2,
  #                      a = head.width / 3.6,
  #                      b = head.height / 3.6,
  #                      angle = 0),
  #                  color = col_headneck)+
  #     # upper arms
  #     geom_ellipse(aes(x0 = 100 - chest.width * 0.45 - dx.up.arm / 2 + x.offset,
  #                      y0 = shoulder.height - dy.up.arm / 2 - 3,
  #                      a = up.arm.width.R / 3.6,
  #                      b = up.arm.length.R / 3.6,
  #                      angle = -angle.up.arm),
  #                  color = col_arm) +
  #    geom_ellipse(aes(x0 = 100 + chest.width * 0.45 + dx.up.arm / 2 + x.offset,
  #                      y0 = shoulder.height - dy.up.arm / 2 - 3,
  #                      a = up.arm.width.L / 3.6,
  #                      b = up.arm.length.L / 3.6,
  #                      angle = angle.up.arm),
  #                  color = col_arm) +
  #         # forearms
  #    geom_ellipse(aes(x0 = 100 - chest.width * 0.45 - dx.up.arm + x.offset,
  #                      y0 = min.y.up.arm - dy.forearm / 2,
  #                      a = forearm.width.R / 3.6,
  #                      b = forearm.length.R / 3.6,
  #                      angle = -angle.forearm),
  #                  color = col_arm) +
  #    geom_ellipse(aes(x0 = 100 + chest.width * 0.45 + dx.up.arm + x.offset,
  #                      y0 = min.y.up.arm - dy.forearm / 2,
  #                      a = forearm.width.L / 3.6,
  #                      b = forearm.length.L / 3.6,
  #                      angle = angle.forearm),
  #                  color = col_arm)+
  #   # hands
  #  geom_ellipse(aes(x0 = 100 - chest.width * 0.45 - dx.up.arm - dx.forearm / 2 + x.offset,
  #                      y0 = min.y.up.arm - dy.forearm - 1,
  #                      a = forearm.width.R / 3.6,
  #                      b = 5,
  #                      angle = 0),
  #                  color = col_arm) +
  #    geom_ellipse(aes(x0 = 100 + chest.width * 0.45 + dx.up.arm + dx.forearm / 2 + x.offset,
  #                      y0 = min.y.up.arm - dy.forearm - 1,
  #                      a = forearm.width.L / 3.6,
  #                      b = 5,
  #                      angle = 0),
  #                  color = col_arm)+
  #     # shoulders
  #     geom_ellipse(aes(x0 = 100 - chest.width * 0.4 + x.offset,
  #                      y0 = shoulder.height - 3,
  #                      a = 1,
  #                      b = 10,
  #                      angle = -90 + shoulder.angle),
  #                  color = col_shoulder) +
  #    geom_ellipse(aes(x0 = 100 + chest.width * 0.4 + x.offset,
  #                      y0 = shoulder.height - 3,
  #                      a = 1,
  #                      b = 10,
  #                      angle = 90 - shoulder.angle),
  #                  color = col_shoulder)
  #     return(p)
}
