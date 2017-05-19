var webpack = require('webpack');
var path = require('path');

var BUILD_DIR = path.resolve(__dirname, 'dist');
var APP_DIR = path.resolve(__dirname);

var config = {
  entry: APP_DIR + '/erbid.client.js',
  output: {
    path: BUILD_DIR,
    filename: 'erbid.client.dist.js'
  },
  module: {
    loaders : [
      {
        test: /\.jsx?/,
        include: APP_DIR,
        exclude: /node_modules/,
        loader: 'babel-loader'
      }
    ]
  },
  plugins: [
    new webpack.ProvidePlugin({
        "React": "react",
    }),
  ]
};

module.exports = config;
