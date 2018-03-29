const path = require('path');

const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const ScriptExtHtmlWebpackPlugin = require('script-ext-html-webpack-plugin');
const OptimizeCssAssetsPlugin = require('optimize-css-assets-webpack-plugin');

const entryPath = path.join(__dirname, 'src/static/index.js');
const outputPath = path.join(__dirname, 'build');

const mode = process.env.npm_lifecycle_event === 'build' ? 'production' : 'development';

let modeSpecificPlugins;
if (mode === 'production') {
  modeSpecificPlugins = [
    new OptimizeCssAssetsPlugin({}),
  ];
} else {
  modeSpecificPlugins = [];
}

module.exports = {
  mode: mode,
  entry: entryPath,
  output: {
    path: outputPath,
    filename: 'static/js/[name].[chunkhash].js',
  },
  module: {
    noParse: /\.elm$/,
    rules: [
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: [{
          loader: 'elm-webpack-loader',
          options: {
            verbose: true,
            warn: true,
            debug: mode !== 'production',
          }
        }]
      },
      {
        test: /\.css$/,
        use: [
          MiniCssExtractPlugin.loader,
          'css-loader',
          'postcss-loader',
        ],
      }
    ]
  },
  plugins: [
    new MiniCssExtractPlugin({
      filename: 'static/css/[name].[chunkhash].css',
    }),
    new HtmlWebpackPlugin({
      template: 'src/static/index.html',
      filename: 'index.html',
      inject: 'head',
    }),
    new ScriptExtHtmlWebpackPlugin({
      defaultAttribute: 'defer',
    }),
    ...modeSpecificPlugins,
  ]
}
