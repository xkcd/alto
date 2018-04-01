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
        test: /\.css$/,
        use: [
          { loader: 'style-loader', options: { singleton: true } },
          { loader: 'css-loader', options: { modules: true } },
          'less-loader',
        ],
      },
      {
        test: /\.js$/,
        exclude: /node_modules/,
        use: ['babel-loader', 'css-literal-loader'],
      },
    ],
  },

  plugins: [
    new webpack.BannerPlugin('alto client by chromako.de'),
  ],
}
