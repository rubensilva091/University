import React from 'react'
import { Line } from 'react-chartjs-2'

function LineChart({ chartData, titleText }) {
  return (
    <div className="chart-container">
      <Line
        data={chartData}
        options={{
          responsive: true,
          plugins: {
            title: {
              display: true,
              text: titleText,
            },
            legend: {
              display: false,
            },
          },
        }}
      />
    </div>
  )
}
export default LineChart
