#!/usr/bin/env bash
set -e

# Trap SIGINT and SIGTERM to stop the Docker container
trap 'echo ""; echo "⚠️  Build interrupted"; docker stop $(docker ps -q --filter ancestor=$IMAGE) 2>/dev/null; exit 130' INT TERM

BUILD_YML="${1:-.build.yml}"

if [ ! -f "$BUILD_YML" ]; then
  echo "Error: Build manifest '$BUILD_YML' not found"
  exit 1
fi

# Check if yq is installed
if ! command -v yq &> /dev/null; then
  echo "Error: yq is required but not installed."
  echo "Install with: brew install yq"
  exit 1
fi

echo "🧪 Testing $BUILD_YML locally using Docker..."
echo ""

# Parse YAML to extract image and convert to Docker format (e.g., alpine/edge -> alpine:edge)
IMAGE=$(yq -r '.image' "$BUILD_YML" | sed 's/\//:/')
if [ -z "$IMAGE" ] || [ "$IMAGE" = "null" ]; then
  echo "Error: No image specified in $BUILD_YML"
  exit 1
fi

echo "📦 Using image: $IMAGE"
echo ""

# Extract packages as a space-separated string
PACKAGES=$(yq -r '.packages[]' "$BUILD_YML" | tr '\n' ' ')

# Build the script that will run in the container
SCRIPT="set -e

echo '📦 Installing system packages...'
apk add --no-cache bash $PACKAGES
"

# Extract each task name and script
TASK_COUNT=$(yq -r '.tasks | length' "$BUILD_YML")
for i in $(seq 0 $((TASK_COUNT - 1))); do
  TASK_NAME=$(yq -r ".tasks[$i] | keys[0]" "$BUILD_YML")
  TASK_SCRIPT=$(yq -r ".tasks[$i].$TASK_NAME" "$BUILD_YML")
  
  SCRIPT+="
echo \"\"
echo \"🔧 Task: $TASK_NAME\"
$TASK_SCRIPT
"
done

SCRIPT+='
echo ""
echo "✅ All tasks completed successfully!"
'

# Run the container with the script, streaming output
# Mount current directory as ~/ocron inside the container
# Use --init to properly handle signals
docker run --rm --init \
  -v "$(pwd):/root/ocron" \
  -w /root \
  "$IMAGE" \
  sh -c "$SCRIPT"
