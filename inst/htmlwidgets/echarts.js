HTMLWidgets.widget({
  name: 'echarts',
  type: 'output',
  factory: function(el, width, height) {
    var sig = echarts.init(el);
    return {
      renderValue: function(x) {
        sig.setOption(x, true);
      },

      resize: function(width, height) {
      }
    };
  }
});
