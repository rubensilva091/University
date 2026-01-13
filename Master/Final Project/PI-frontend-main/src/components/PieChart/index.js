import React from 'react'
import { Pie } from 'react-chartjs-2'

function PieChart({ chartData, titleText }) {
  return (
    <div className="chart-container">
      <Pie
        data={chartData}
        options={{
          responsive: true,
          plugins: {
            title: {
              display: true,
              text: titleText,
            },
          },
        }}
      />
    </div>
  )
}
export default PieChart
