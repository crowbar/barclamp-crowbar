# Disable root element in the JSON responses for POROs,
# i.e. keep the behavior the same as in cloud 4
ActiveModel::Serializer.root = false
ActiveModel::ArraySerializer.root = false
