body {
  {% if darken %}
  background-color: #222222;
  color: blue;
  {% else %}
  background-color: white;
  color: back;
  {% endif %}
}

#content {
  width: {{ pageWidth }};
  margin: 0 auto;
}
