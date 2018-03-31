import path from 'path'
import webpack from 'webpack'

export default {
  entry: {
    comic: './src/index.js',
  },

  output: {
    filename: '[name].js',
    path: path.join(__dirname, 'build'),
  },

  module: {
    rules: [
      {
        test: /\.js$/,
        exclude: /node_modules/,
        loader: 'babel-loader',
      },
    ],
  },

  plugins: [
    new webpack.BannerPlugin('alto client by chromako.de'),
  ],
}
