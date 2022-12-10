REMOTE_HOST="cloud"
REMOTE_DIR="~/sites/stats/public_html/testy-turtle"
REMOTE_DEST=$REMOTE_HOST:$REMOTE_DIR

echo "Uploading new changes to remote server..."
echo
rsync -crvP --delete _site/ $REMOTE_DEST
