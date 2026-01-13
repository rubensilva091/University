import React, { useEffect, useState } from 'react'

export const CheckIcon = ({ height }) => {
  const [scale, setScale] = useState(0.9)

  useEffect(() => {
    const interval = setInterval(() => {
      setScale((prev) => (prev < 1 ? 1.05 : 0.9))
    }, 1000)

    return () => clearInterval(interval)
  }, [])

  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width="100%"
      height={height}
      viewBox="0 0 822 687"
    >
      <path
        id="background"
        fill="limegreen"
        opacity="10%"
        style={{
          transform: `scale(${scale})`,
          transformOrigin: 'center',
          transition: 'transform 1.5s ease-in-out',
        }}
        d="M 404,666.67
           C 192,660 90.67,506 84.67,340.67
             82.67,189.33 206.67,23.33 409.33,19.33
             610.67,22 725.33,188.67 730,330.67
             734,487.33 620.67,662.67 404,666.67 Z"
      />
      <path
        id="check"
        fill="limegreen"
        d="M 497,126.44
           C 525.33,140.67 509.33,173.33 482.67,163.67
             326.67,97.33 128.67,266 250.33,455.33
             391.02,628.91 615.79,498.19 600,327.33
             598.33,291.33 642.33,291 643.33,326
             652.10,441.80 559.02,591.05 385,578
             249,563 182.50,456 171.50,367.50
             152.47,204.35 320.58,55.29 497,126.44 Z
           M 393.65,386
           C 409.65,368.83 612.81,154.21 641.32,125.55
             660.45,108.27 687.50,132.75 675.32,150.55
             653.71,175.32 456.77,380.82 404.27,435.82
             398.18,441.55 389.09,441.36 384.12,436.75
             347.25,402.25 291.28,343.76 281.50,333.50
             263.50,313.25 291.62,287.50 311.62,306.50
             323.67,318.29 374.39,367.39 393.65,386 Z"
      />
    </svg>
  )
}
